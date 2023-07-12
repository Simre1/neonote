module NeoNote.Run where

import Control.Monad (forM_, when)
import Data.List.NonEmpty as NE (NonEmpty (..), fromList, zip)
import Data.Text (Text)
import Effectful
import Effectful.Error.Dynamic
import NeoNote.Actions qualified as Action
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Error
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Search
import NeoNote.Store.Database (Database, runDatabase)
import NeoNote.Store.Files (Files, runFiles)
import NeoNote.Store.Note
import NeoNote.Time
import NeoNote.UI
import NeoNote.UI.Picker (PickedAction (..))
import NeoNote.UI.Prompt

type AppEffects = [UI, NoteSearch, NoteStore, GetTime, MakeId, Database, Files, Error NeoNoteError, Log, GetConfiguration, IOE]

runNeoNote :: IO ()
runNeoNote = runIO $ do
  action <- getActionFromArguments
  handleAction action

runIO :: Eff AppEffects () -> IO ()
runIO =
  runEff
    . injectConfiguration mempty
    . runLog
    . runNeoNoteError
    . runFiles
    . runDatabase
    . runMakeId
    . runGetTime
    . runNoteStore
    . runNoteSearch
    . runUI

handleAction :: Action.Action -> Eff AppEffects ()
handleAction action = case action of
  Action.CreateNote skipEditor initialText -> handleCreateNote initialText skipEditor
  Action.EditNote noteFilter amount searchText -> editNote noteFilter amount searchText
  Action.PickNote noteFilter searchText -> pickNote noteFilter searchText
  Action.DeleteNote noteFilter amount searchText -> handleDeleteNote' noteFilter amount searchText
  Action.ViewNote noteFilter amount searchText -> viewNote noteFilter amount searchText
  Action.ListNotes noteFilter attributesToShow showAmount orderBy searchText ->
    listNotes noteFilter searchText attributesToShow showAmount orderBy
  Action.ScanNotes -> scanNotes

handleCreateNote :: (UI :> es, Log :> es, NoteStore :> es) => Text -> Bool -> Eff es ()
handleCreateNote initialText skipEditor = do
  createNote $ \noteInfo -> do
    let initialNoteContent = NoteContent initialText

    (noteContent :| _) <-
      if skipEditor
        then pure $ pure initialNoteContent
        else editor $ pure (noteInfo, initialNoteContent)

    if hasContent noteContent
      then do
        logMessage NoteCreated
        pure $ Just noteContent
      else do
        logMessage NoteEmpty
        pure Nothing

editNote :: (UI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, NoteSearch :> es) => NoteFilter -> Int -> Text -> Eff es ()
editNote noteFilter amount searchTerm = do
  selectedNotes <- take amount <$> searchNotes noteFilter searchTerm
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    (a : as) -> do
      let noteInfos = a :| as
      noteContents <- traverse readNote noteInfos
      newNoteContents <- editor $ NE.zip noteInfos noteContents

      forM_ (NE.zip noteInfos newNoteContents) $ \(noteInfo, newNoteContent) ->
        if hasContent newNoteContent
          then do
            writeNote noteInfo newNoteContent
            logMessage NoteEdited
          else do
            logMessage NoteEmpty

pickNote :: (UI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Eff es ()
pickNote noteFilter searchTerm = do
  pick noteFilter searchTerm $ \case
    (Just (PickedEdit noteInfo)) -> do
      noteContent <- readNote noteInfo
      (newNoteContent :| _) <- editor $ pure (noteInfo, noteContent)
      if hasContent newNoteContent
        then do
          writeNote noteInfo newNoteContent
          logMessage NoteEdited
        else do
          logMessage NoteEmpty
      pure False
    (Just (PickedDelete noteInfo)) -> do
      noteContent <- readNote noteInfo
      answer <- prompt (AreYouSureDeletion ((noteInfo, noteContent) :| []))
      when answer $ do
        deleteNote noteInfo
      pure True
    (Just (PickedView noteInfo)) -> do
      noteContent <- readNote noteInfo
      displayNote noteInfo noteContent
      pure False
    _ -> False <$ logMessage NoMatchingNote

handleDeleteNote' :: (UI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, NoteSearch :> es) => NoteFilter -> Int -> Text -> Eff es ()
handleDeleteNote' noteFilter amount searchTerm = do
  selectedNotes <- take amount <$> searchNotes noteFilter searchTerm
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    noteInfos -> do
      noteContents <- traverse readNote noteInfos
      answer <-
        prompt
          ( AreYouSureDeletion
              (NE.fromList $ Prelude.zip noteInfos noteContents)
          )
      when answer $ do
        mapM_ deleteNote noteInfos

viewNote :: (UI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, NoteSearch :> es) => NoteFilter -> Int -> Text -> Eff es ()
viewNote noteFilter amount searchTerm = do
  selectedNotes <- take amount <$> searchNotes noteFilter searchTerm
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    noteInfos -> do
      forM_ noteInfos $ \noteInfo -> do
        noteContent <- readNote noteInfo
        displayNote noteInfo noteContent

listNotes :: (UI :> es, Error NeoNoteError :> es) => NoteFilter -> Text -> [NoteAttribute] -> Int -> OrderBy NoteAttribute -> Eff es ()
listNotes noteFilter search noteAttributes showAmount orderBy = do
  displayNotes noteFilter search orderBy showAmount noteAttributes

scanNotes :: (IOE :> es) => Eff es ()
scanNotes = liftIO $ putStrLn "Sorry, not yet implemented"
