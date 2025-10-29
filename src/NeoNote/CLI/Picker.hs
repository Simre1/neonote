module NeoNote.CLI.Picker where

import Brick
import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Edit qualified as E
import Control.Applicative
import Control.Exception (catch)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.List.SafeIndex
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Dynamic
import GHC.Generics
import Graphics.Vty qualified as Vty
import NeoNote.Error
import NeoNote.Note.Highlight (Highlight, highlight)
import NeoNote.Note.Note
import NeoNote.Time
import Optics.Core

data PickedAction
  = PickedView NoteInfo
  | PickedEdit NoteInfo
  | PickedDelete NoteInfo
  | PickedNothing
  deriving (Show, Eq, Generic, Ord)

data UINoteHandler = UINoteHandler
  { callbacks :: PickerCallbacks IO,
    highlight :: Note -> IO Text
  }
  deriving (Generic)

data PickerCallbacks m = PickerCallbacks
  { findNotes :: (Text -> m [NoteInfo]),
    getNoteContent :: NoteId -> m NoteContent,
    handlePickedAction :: PickedAction -> m Bool
  }
  deriving (Generic)

picker ::
  (IOE :> es, Error NeoNoteError :> es, Highlight :> es) =>
  Text -> PickerCallbacks (Eff es) -> Eff es ()
picker initialText callbacks = do
  withRunInIO $ \unlift -> do
    let noteHandler =
          UINoteHandler
            { highlight = \(Note ni nc) -> unlift $ highlight ni nc,
              callbacks =
                PickerCallbacks
                  { findNotes = unlift . (callbacks ^. #findNotes),
                    getNoteContent = unlift . (callbacks ^. #getNoteContent),
                    handlePickedAction = unlift . (callbacks ^. #handlePickedAction)
                  }
            }
    catch
      ( do
          let singlePick previousUIState = do
                currentUIState <- refreshNotes noteHandler previousUIState
                uiState <- pickerApp noteHandler currentUIState
                continue <- unlift $ callbacks ^. #handlePickedAction $ uiState ^. #result
                when continue $
                  singlePick $
                    uiState & #result .~ PickedNothing
          singlePick (makeInitialState initialText)
      )
      $ \e -> do
        unlift $ throwError $ PickerCrashed e

makeInitialState :: Text -> UIState
makeInitialState initialSearchTerm = do
  UIState [] 0 Nothing (E.editorText "editor" Nothing initialSearchTerm) PickedNothing

pickerApp :: UINoteHandler -> UIState -> IO UIState
pickerApp noteHandler = defaultMain app
  where
    app :: App UIState () Text
    app =
      App
        { appDraw = pure . drawUI,
          appChooseCursor = const listToMaybe,
          appHandleEvent = appEvent,
          appStartEvent = pure (),
          appAttrMap = const theMap
        }

    drawUI :: UIState -> T.Widget Text
    drawUI st =
      (notesList <+> previewNote)
        <=> (hBorder <=> (searchbar <+> infos))
      where
        notesList =
          padTop Max $ (<+> vBorder) $ padAll 1 $ case imap drawItem (st ^. #filteredNotes) of
            -- column width is 17 chars
            [] -> txt "No matching notes"
            items -> foldl1 (<=>) items
        drawItem index noteInfo =
          (if index == st ^. #position then withAttr selectedAttr else id) $
            txt [__i|#{noteIdToText $ noteInfo ^. #id} #{formatDate (noteInfo ^. #modified)}\n#{T.take 30 $ concatFields $ noteInfo ^. #fields}|]
        searchbar =
          padLeft (Pad 1) (padRight Max (txt "Search: " <+> hLimit 30 (vLimit 1 $ E.renderEditor (txt . T.unlines) True (st ^. #searchTerm))))
        previewNote = padBottom Max $ padAll 1 $ padRight Max $ txt $ fromMaybe "" $ do
          coerce $ st ^. #previewedNote
        infos = txt ""

    appEvent :: T.BrickEvent Text () -> T.EventM Text UIState ()
    appEvent ev = case ev of
      (T.VtyEvent (Vty.EvKey Vty.KEsc [])) -> M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) -> M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'z') [Vty.MCtrl])) -> M.halt
      (T.VtyEvent (Vty.EvKey Vty.KEnter [])) -> do
        modify $ \state ->
          state
            & #result
            .~ (maybe PickedNothing PickedEdit $ selectedNoteInfo state)
        M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'x') [Vty.MCtrl])) -> do
        modify $ \state ->
          state
            & #result
            .~ (maybe PickedNothing PickedEdit $ selectedNoteInfo state)
        M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'y') [Vty.MCtrl])) -> do
        modify $ \state ->
          state
            & #result
            .~ (maybe PickedNothing PickedEdit $ selectedNoteInfo state)
        M.halt
      (T.VtyEvent (Vty.EvKey Vty.KUp [])) -> do
        modify $ #position %~ max 0 . pred
        updatePreview
      (T.VtyEvent (Vty.EvKey Vty.KDown [])) -> do
        modify $ \state -> state & #position %~ min (length (state ^. #filteredNotes) - 1) . succ
        updatePreview
      _ -> do
        zoom (toLensVL #searchTerm) $ E.handleEditorEvent ev
        updateNotes
        updatePreview
      where
        updatePreview :: EventM Text UIState ()
        updatePreview = do
          position <- view #position <$> get
          filteredNotes <- view #filteredNotes <$> get
          let noteInfo = filteredNotes !? position
          noteContent <- liftIO $ traverse (noteHandler ^. #callbacks % #getNoteContent) (noteInfo ^. mapping #id)
          highlightedContent <- liftIO $ sequenceA $ liftA (noteHandler ^. #highlight) (Note <$> noteInfo <*> noteContent)
          modify $ #previewedNote .~ highlightedContent
        updateNotes :: EventM Text UIState ()
        updateNotes = do
          st <- get
          filteredNotes <- liftIO $ (noteHandler ^. #callbacks % #findNotes) (getSearchTerm st)
          put $
            st
              & #filteredNotes
              .~ filteredNotes
              & #position
              %~ max 0
              . min (length filteredNotes - 1)

selectedAttr :: A.AttrName
selectedAttr = attrName "selected"

theMap :: A.AttrMap
theMap =
  A.attrMap
    Vty.defAttr
    [ (selectedAttr, Vty.black `on` Vty.white)
    ]

getSearchTerm :: UIState -> Text
getSearchTerm = T.strip . T.unlines . E.getEditContents . view #searchTerm

selectedNoteInfo :: UIState -> Maybe NoteInfo
selectedNoteInfo st = (st ^. #filteredNotes) !? (st ^. #position)

refreshNotes :: UINoteHandler -> UIState -> IO UIState
refreshNotes noteHandler st = do
  filteredNotes <- (noteHandler ^. #callbacks % #findNotes) (mconcat $ E.getEditContents $ st ^. #searchTerm)
  let noteInfo = filteredNotes !? (st ^. #position)
  noteContent <- liftIO $ traverse (noteHandler ^. #callbacks % #getNoteContent) (noteInfo ^. mapping #id)
  highlightedContent <- sequenceA $ liftA (noteHandler ^. #highlight) $ Note <$> noteInfo <*> noteContent
  pure $
    st
      & (#previewedNote .~ highlightedContent)
      & (#filteredNotes .~ filteredNotes)

data UIState = UIState
  { filteredNotes :: [NoteInfo],
    position :: Int,
    previewedNote :: Maybe Text,
    searchTerm :: E.Editor Text Text,
    result :: PickedAction
  }
  deriving (Generic, Show)
