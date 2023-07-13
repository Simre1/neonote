module NeoNote.UI.Picker where

import Brick
import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Edit qualified as E
import Control.Applicative (Applicative (..))
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
import NeoNote.Log
import NeoNote.Note.Highlight (highlight, Highlight)
import NeoNote.Note.Note
import NeoNote.Search
import NeoNote.Store.Note
import NeoNote.Time (timeToString)
import Optics.Core

data PickedAction = PickedView NoteInfo | PickedEdit NoteInfo | PickedDelete NoteInfo deriving (Show, Eq, Generic, Ord)

picker :: (IOE :> es, NoteStore :> es, Log :> es, Error NeoNoteError :> es, NoteSearch :> es, Highlight :> es) => NoteFilter -> Text -> (Maybe PickedAction -> Eff es Bool) -> Eff es ()
picker noteFilter initialText handlePickedAction = do
  withRunInIO $ \unlift -> do
    let highlightIO ni nc = unlift $ highlight ni nc
    unlift $ withNoteSearchHandle $ \noteSearchHandle -> do
      catch
        ( do
            let singlePick previousUIState = do
                  currentUIState <- refreshNotes noteSearchHandle highlightIO previousUIState
                  uiState <- pickerApp noteSearchHandle highlightIO currentUIState
                  continue <- unlift $ handlePickedAction $ uiState ^. #result
                  when continue $
                    singlePick $
                      uiState & #result .~ Nothing
            singlePick (makeInitialState noteFilter initialText)
        )
        $ \e -> do
          unlift $ throwError $ SearchUICrashed e

makeInitialState :: NoteFilter -> Text -> UIState
makeInitialState initialNoteFilter initialSearchTerm = do
  UIState [] 0 Nothing (E.editorText "editor" Nothing initialSearchTerm) initialNoteFilter Nothing

pickerApp :: NoteSearchHandle -> (NoteInfo -> NoteContent -> IO Text) -> UIState -> IO UIState
pickerApp noteSearchHandle highlightIO = defaultMain app
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
            [] -> txt "No notes match your query"
            items -> foldl1 (<=>) items
        drawItem index noteInfo =
          (if index == st ^. #position then withAttr selectedAttr else id) $
            txt [__i| #{timeToString $ noteInfo ^. #modified}\n #{T.take 30 $ concatTags $ noteInfo ^. #tags}  |]
        searchbar =
          padLeft (Pad 1) (padRight Max (txt "Search: " <+> hLimit 30 (vLimit 1 $ E.renderEditor (txt . T.unlines) True (st ^. #searchTerm))))
        previewNote = padBottom Max $ padAll 1 $ padRight Max $ txt $ fromMaybe "No matched note" $ do
          coerce $ st ^. #previewedNote
        infos = txt "edit: enter, view: ctrl-y, delete: ctrl-x, exit: esc/ctrl-c"

    appEvent :: T.BrickEvent Text () -> T.EventM Text UIState ()
    appEvent ev = case ev of
      (T.VtyEvent (Vty.EvKey Vty.KEsc [])) -> M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) -> M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'z') [Vty.MCtrl])) -> M.halt
      (T.VtyEvent (Vty.EvKey Vty.KEnter [])) -> do
        modify $ \state ->
          state
            & #result .~ (PickedEdit <$> selectedNoteInfo state)
        M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'x') [Vty.MCtrl])) -> do
        modify $ \state ->
          state
            & #result .~ (PickedDelete <$> selectedNoteInfo state)
        M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'y') [Vty.MCtrl])) -> do
        modify $ \state ->
          state
            & #result .~ (PickedView <$> selectedNoteInfo state)
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
          noteContent <- liftIO $ traverse (noteSearchHandle ^. #getNoteContent) noteInfo
          highlightedContent <- liftIO $ sequenceA $ liftA2 highlightIO noteInfo noteContent
          modify $ #previewedNote .~ highlightedContent
        updateNotes :: EventM Text UIState ()
        updateNotes = do
          st <- get
          filteredNotes <- liftIO $ (noteSearchHandle ^. #searchNotes) (st ^. #noteFilter) (getSearchTerm st)
          put $
            st
              & #filteredNotes .~ filteredNotes
              & #position %~ max 0 . min (length filteredNotes - 1)

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

refreshNotes :: NoteSearchHandle -> (NoteInfo -> NoteContent -> IO Text) -> UIState -> IO UIState
refreshNotes noteSearchHandle highlightIO st = do
  filteredNotes <- liftIO $ (noteSearchHandle ^. #searchNotesNoCache) (st ^. #noteFilter) (getSearchTerm st)
  let noteInfo = filteredNotes !? (st ^. #position)
  noteContent <- liftIO $ traverse (noteSearchHandle ^. #getNoteContent) noteInfo
  highlightedContent <- sequenceA $ liftA2 highlightIO noteInfo noteContent
  pure $
    st
      & #previewedNote .~ highlightedContent
      & #filteredNotes .~ filteredNotes

data UIState = UIState
  { filteredNotes :: [NoteInfo],
    position :: Int,
    previewedNote :: Maybe Text,
    searchTerm :: E.Editor Text Text,
    noteFilter :: NoteFilter,
    result :: Maybe PickedAction
  }
  deriving (Generic, Show)
