module NeoNote.UI.Search where

import Brick
import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Edit qualified as E
import Control.Monad (zipWithM)
import Data.Coerce (coerce)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import GHC.Generics
import Graphics.Vty qualified as Vty
import NeoNote.Note.Note
import NeoNote.Store.Database
import NeoNote.Store.Files
import Optics.Core
import Text.Fuzzy qualified as Fuzzy
import Data.List.SafeIndex
import Control.Exception (catch)
import NeoNote.Log
import Effectful.Error.Dynamic
import NeoNote.Error

fuzzySearch :: (IOE :> es, Database :> es, Files :> es, Log :> es, Error NeoNoteError :> es) => NoteFilter -> Text -> Eff es (Maybe NoteId)
fuzzySearch noteFilter initialText = do
  
  noteIds <- findNotes noteFilter
  noteInfos <- traverse getNoteInfo noteIds
  noteContents <- zipWithM readNote noteIds noteInfos

  let notes = zip (zip noteIds noteInfos) (coerce <$> noteContents)
      noteMap = M.fromList $ zip noteIds noteContents
      getNoteContent noteId = pure $ noteMap M.! noteId
      searchNotes = pure . fuzzySearchNotes notes
  
  withRunInIO $ \unlift -> catch (searchApp getNoteContent searchNotes initialText) $ \e -> do
    unlift $ throwError $ SearchUICrashed e
    

fuzzySearchNotes :: [(a, NoteContent)] -> Text -> [a]
fuzzySearchNotes notes searchTerm =
  fst . Fuzzy.original
    <$> Fuzzy.filter
      searchTerm
      notes
      ""
      ""
      (coerce . snd)
      False

searchApp :: (NoteId -> IO NoteContent) -> (Text -> IO [(NoteId, NoteInfo)]) -> Text -> IO (Maybe NoteId)
searchApp getNoteContent searchNotes initialSearchTerm = do
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
      filteredNotes <- searchNotes initialSearchTerm
      noteContent <- liftIO $ traverse (getNoteContent . fst) $ filteredNotes !? 0
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
        drawItem i item = (if i == st ^. #position then withAttr selectedAttr else id) $ txt (uncurry noteFileName item)
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
        filteredNotes <- liftIO $ searchNotes searchTerm
        modify $ #filteredNotes .~ filteredNotes
        modify $ #position %~ max 0 . min (length filteredNotes - 1)
        updatePreview
      where
        updatePreview :: EventM Text UIState ()
        updatePreview = do
          position <- view #position <$> get 
          filteredNotes <- view #filteredNotes <$> get
          noteContent <- liftIO $ traverse (getNoteContent . fst) $ filteredNotes !? position
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