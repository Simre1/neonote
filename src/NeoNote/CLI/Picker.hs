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
import NeoNote.Log
import NeoNote.Note.Highlight (Highlight, highlight)
import NeoNote.Note.Note
import NeoNote.Note.Parse
import NeoNote.Store.Note
import NeoNote.Time
import Optics.Core

data PickedAction = PickedView NoteInfo | PickedEdit NoteInfo | PickedDelete NoteInfo deriving (Show, Eq, Generic, Ord)

data UINoteHandler = UINoteHandler
  { getNoteContent :: NoteId -> IO NoteContent,
    searchNotes :: NoteFilter -> IO [NoteInfo],
    highlight :: Note -> IO Text,
    parseNoteFilter :: Text -> IO (Maybe NoteFilter)
  }
  deriving (Generic)

picker :: (IOE :> es, GetTime :> es, NoteStore :> es, Log :> es, Error NeoNoteError :> es, Highlight :> es) => NoteFilter -> Text -> (Maybe PickedAction -> Eff es Bool) -> Eff es ()
picker noteFilter initialText handlePickedAction = do
  withRunInIO $ \unlift -> do
    let noteHandler =
          UINoteHandler
            { highlight = \(Note ni nc) -> unlift $ highlight ni nc,
              searchNotes = \nf -> unlift $ findNotes nf >>= traverse readNoteInfo,
              getNoteContent = \noteId -> unlift $ fmap (^. #content) (readNote noteId),
              parseNoteFilter = \text -> do
                time <- unlift getCurrentTime
                pure $ case parseNoteFilter time text of
                  Right x -> Just x
                  Left _err -> Nothing
            }
    do
      catch
        ( do
            let singlePick previousUIState = do
                  currentUIState <- refreshNotes noteHandler previousUIState
                  uiState <- pickerApp noteHandler currentUIState
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
            [] -> txt "No notes match your query"
            items -> foldl1 (<=>) items
        drawItem index noteInfo =
          (if index == st ^. #position then withAttr selectedAttr else id) $
            txt [__i| #{formatTime $ noteInfo ^. #modified}\n #{T.take 30 $ concatTags $ noteInfo ^. #tags}  |]
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
            & #result
            .~ (PickedEdit <$> selectedNoteInfo state)
        M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'x') [Vty.MCtrl])) -> do
        modify $ \state ->
          state
            & #result
            .~ (PickedDelete <$> selectedNoteInfo state)
        M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'y') [Vty.MCtrl])) -> do
        modify $ \state ->
          state
            & #result
            .~ (PickedView <$> selectedNoteInfo state)
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
          noteContent <- liftIO $ traverse (noteHandler ^. #getNoteContent) (noteInfo ^. mapping #id)
          highlightedContent <- liftIO $ sequenceA $ liftA (noteHandler ^. #highlight) (Note <$> noteInfo <*> noteContent)
          modify $ #previewedNote .~ highlightedContent
        updateNotes :: EventM Text UIState ()
        updateNotes = do
          st <- get
          maybeNewNoteFilter <- liftIO $ (noteHandler ^. #parseNoteFilter) (getSearchTerm st)

          case maybeNewNoteFilter of
            Just newNoteFilter -> do
              filteredNotes <- liftIO $ (noteHandler ^. #searchNotes) newNoteFilter
              put $
                st
                  & #filteredNotes
                  .~ filteredNotes
                  & #position
                  %~ max 0
                  . min (length filteredNotes - 1)
            Nothing -> pure ()

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
  filteredNotes <- (noteHandler ^. #searchNotes) (st ^. #noteFilter)
  let noteInfo = filteredNotes !? (st ^. #position)
  noteContent <- liftIO $ traverse (noteHandler ^. #getNoteContent) (noteInfo ^. mapping #id)
  highlightedContent <- sequenceA $ liftA (noteHandler ^. #highlight) $ Note <$> noteInfo <*> noteContent
  pure $
    st
      & #previewedNote
      .~ highlightedContent
      & #filteredNotes
      .~ filteredNotes

data UIState = UIState
  { filteredNotes :: [NoteInfo],
    position :: Int,
    previewedNote :: Maybe Text,
    searchTerm :: E.Editor Text Text,
    noteFilter :: NoteFilter,
    result :: Maybe PickedAction
  }
  deriving (Generic, Show)
