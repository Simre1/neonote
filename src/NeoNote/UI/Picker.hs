module NeoNote.UI.Picker where

import Brick
import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Edit qualified as E
import Control.Exception (catch)
import Data.Coerce (coerce)
import Data.List.SafeIndex
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Dynamic
import GHC.Generics
import Graphics.Vty qualified as Vty
import NeoNote.Error
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Search
import NeoNote.Store.Database
import NeoNote.Store.Files
import Optics.Core
import NeoNote.Time (timeToString)
import Data.String.Interpolate

picker :: (IOE :> es, Database :> es, Files :> es, Log :> es, Error NeoNoteError :> es) => NoteFilter -> Text -> Eff es (Maybe NoteId)
picker noteFilter initialText = do
  preparedSearch <- prepareSearch noteFilter

  withRunInIO $ \unlift -> do
    let preparedSearchIO = mapPreparedSearch unlift preparedSearch
    catch (pickerApp preparedSearchIO initialText) $ \e -> do
      unlift $ throwError $ SearchUICrashed e

pickerApp :: PreparedSearch IO -> Text -> IO (Maybe NoteId)
pickerApp preparedSearch initialSearchTerm = do
  initialState <- makeInitialState
  uiState <- defaultMain app initialState
  pure $ uiState ^. #result
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
    makeInitialState :: IO UIState
    makeInitialState = do
      filteredNotes <- (preparedSearch ^. #searchNotes) initialSearchTerm
      noteContent <- liftIO $ traverse ((preparedSearch ^. #getNoteContent) . fst) $ filteredNotes !? 0
      pure $ UIState filteredNotes 0 noteContent (E.editorText "editor" Nothing initialSearchTerm) Nothing

    drawUI :: UIState -> T.Widget Text
    drawUI st =
      (notesList <+> previewNote)
        <=> searchbar
      where
        notesList =
          padTop Max $ (<+> vBorder) $ padAll 1 $ case imap drawItem (st ^. #filteredNotes) of
            [] -> txt "No notes match your query"
            items -> foldl1 (<=>) items
        drawItem i (_, noteInfo) =
          (if i == st ^. #position then withAttr selectedAttr else id) $
            txt [__i| #{timeToString $ noteInfo ^. #modified}\n #{T.take 30 $ concatTags $ noteInfo ^. #tags}  |]
        searchbar =
          hBorder
            <=> padLeft (Pad 1) (padRight Max (txt "Search: " <+> hLimit 30 (vLimit 1 $ E.renderEditor (txt . T.unlines) True (st ^. #searchTerm))))
        previewNote = padBottom Max $ padAll 1 $ padRight Max $ txt $ fromMaybe "No matched note" $ do
          coerce $ st ^. #previewedNote

    appEvent :: T.BrickEvent Text e -> T.EventM Text UIState ()
    appEvent ev = case ev of
      (T.VtyEvent (Vty.EvKey Vty.KEsc [])) -> M.halt
      (T.VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) -> M.halt
      (T.VtyEvent (Vty.EvKey Vty.KEnter [])) -> do
        modify $ \state ->
          state
            & #result .~ do
              (noteId, _) <- (state ^. #filteredNotes) !? (state ^. #position)
              pure noteId
        M.halt
      (T.VtyEvent (Vty.EvKey Vty.KUp [])) -> do
        modify $ #position %~ max 0 . pred
        updatePreview
      (T.VtyEvent (Vty.EvKey Vty.KDown [])) -> do
        modify $ \state -> state & #position %~ min (length (state ^. #filteredNotes) - 1) . succ
        updatePreview
      _ -> do
        zoom (toLensVL #searchTerm) $ E.handleEditorEvent ev
        searchTerm <- T.strip . T.unlines . E.getEditContents . view #searchTerm <$> get
        filteredNotes <- liftIO $ (preparedSearch ^. #searchNotes) searchTerm
        modify $ #filteredNotes .~ filteredNotes
        modify $ #position %~ max 0 . min (length filteredNotes - 1)
        updatePreview
      where
        updatePreview :: EventM Text UIState ()
        updatePreview = do
          position <- view #position <$> get
          filteredNotes <- view #filteredNotes <$> get
          noteContent <- liftIO $ traverse ((preparedSearch ^. #getNoteContent) . fst) $ filteredNotes !? position
          modify $ #previewedNote .~ noteContent

    selectedAttr :: A.AttrName
    selectedAttr = attrName "selected"

    theMap :: A.AttrMap
    theMap =
      A.attrMap
        Vty.defAttr
        [ (selectedAttr, Vty.black `on` Vty.white)
        ]

data UIState = UIState
  { filteredNotes :: [(NoteId, NoteInfo)],
    position :: Int,
    previewedNote :: Maybe NoteContent,
    searchTerm :: E.Editor Text Text,
    result :: Maybe NoteId
  }
  deriving (Generic, Show)
