module NeoNote.Run where

import Control.Monad (when)
import Data.List.SafeIndex
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
import NeoNote.Store.Note
import NeoNote.Time
import NeoNote.UI
import NeoNote.UI.Prompt
import Optics.Core ((^.))
import NeoNote.Store.Database (Database, runDatabase)
import NeoNote.Store.Files (Files, runFiles)

type AppEffects = [UI, NoteStore, GetTime, MakeId, Database, Files, Error NeoNoteError, Log, GetConfiguration, IOE]

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
    . runUI

handleAction :: Action.Action -> Eff AppEffects ()
handleAction action = case action of
  Action.CreateNote skipEditor initialText -> handleCreateNote initialText skipEditor
  Action.EditNote noteFilter skipPicker searchText -> editNote noteFilter searchText skipPicker
  Action.DeleteNote noteFilter skipPicker searchText -> handleDeleteNote' noteFilter searchText skipPicker
  Action.ViewNote noteFilter skipPicker searchText -> viewNote noteFilter searchText skipPicker
  Action.ListNotes noteFilter attributesToShow showAmount orderBy searchText ->
    listNotes noteFilter searchText attributesToShow showAmount orderBy
  Action.ScanNotes -> scanNotes

handleCreateNote :: (UI :> es, Log :> es, NoteStore :> es) => Text -> Bool -> Eff es ()
handleCreateNote initialText skipEditor = do
  createNote $ \noteInfo -> do
    let initialNoteContent = NoteContent initialText

    noteContent <-
      if skipEditor
        then pure initialNoteContent
        else editor noteInfo initialNoteContent

    if hasContent noteContent
      then do
        logMessage NoteCreated
      else do
        logMessage NoteEmpty
    pure $ Just noteContent

editNote :: (UI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Bool -> Eff es ()
editNote noteFilter searchTerm skipPicker = do
  maybeSelectedNoteInfo <-
    if skipPicker
      then do
        preparedSearch <- prepareSearch noteFilter
        notes <- (preparedSearch ^. #searchNotes) searchTerm
        pure $ notes !? 0
      else pick noteFilter searchTerm
  case maybeSelectedNoteInfo of
    Just noteInfo -> do
      noteContent <- readNote noteInfo
      newNoteContent <- editor noteInfo noteContent

      if hasContent newNoteContent
        then do
          writeNote noteInfo newNoteContent
          logMessage NoteEdited
        else do
          logMessage NoteEmpty
    _ -> logMessage NoMatchingNote

handleDeleteNote' :: (UI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Bool -> Eff es ()
handleDeleteNote' noteFilter searchTerm skipPicker = do
  maybeSelectedNoteId <-
    if skipPicker
      then do
        preparedSearch <- prepareSearch noteFilter
        notes <- (preparedSearch ^. #searchNotes) searchTerm
        pure $ notes !? 0
      else pick noteFilter searchTerm
  case maybeSelectedNoteId of
    Just noteId -> do
      answer <- prompt AreYouSureDeletion
      when answer $ do
        deleteNote noteId
    _ -> logMessage NoMatchingNote

viewNote :: (UI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Bool -> Eff es ()
viewNote noteFilter searchTerm skipPicker = do
  maybeSelectedNoteInfo <-
    if skipPicker
      then do
        preparedSearch <- prepareSearch noteFilter
        notes <- (preparedSearch ^. #searchNotes) searchTerm
        pure $ notes !? 0
      else pick noteFilter searchTerm
  case maybeSelectedNoteInfo of
    Just noteInfo -> do
      noteContent <- readNote noteInfo
      displayNote noteInfo noteContent
    _ -> logMessage NoMatchingNote

listNotes :: (UI :> es, Error NeoNoteError :> es) => NoteFilter -> Text -> [NoteAttribute] -> Int -> OrderBy NoteAttribute -> Eff es ()
listNotes noteFilter search noteAttributes showAmount orderBy = do
  displayNotes noteFilter search orderBy showAmount noteAttributes

scanNotes :: (IOE :> es) => Eff es ()
scanNotes = liftIO $ putStrLn "Sorry, not yet implemented"
