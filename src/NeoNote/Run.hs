module NeoNote.Run where

import Control.Monad (when)
import Data.List.SafeIndex
import Data.Text (Text)
import Effectful
import Effectful.Error.Dynamic
import NeoNote.Actions
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Error
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Note.Parse (extractTags)
import NeoNote.Search
import NeoNote.Store.Database
import NeoNote.Store.Files
import NeoNote.Time
import NeoNote.UI
import NeoNote.UI.Prompt
import Optics.Core ((^.))

type AppEffects = [UI, GetTime, MakeId, Database, Files, Error NeoNoteError, Log, GetConfiguration, IOE]

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
    . runUI

handleAction :: Action -> Eff AppEffects ()
handleAction action = case action of
  CreateNote skipEditor initialText -> createNote initialText skipEditor
  EditNote noteFilter skipPicker searchText -> editNote noteFilter searchText skipPicker
  DeleteNote noteFilter skipPicker searchText -> deleteNote noteFilter searchText skipPicker
  ViewNote noteFilter skipPicker searchText -> viewNote noteFilter searchText skipPicker
  ListNotes noteFilter attributesToShow showAmount orderBy searchText ->
    listNotes noteFilter searchText attributesToShow showAmount orderBy
  ScanNotes -> scanNotes

createNote :: (Database :> es, GetConfiguration :> es, GetTime :> es, MakeId :> es, UI :> es, Files :> es, Log :> es) => Text -> Bool -> Eff es ()
createNote initialText skipEditor = do
  noteId <- makeNoteId
  noteInfo <- makeNewNoteInfo
  let initialNoteContent = NoteContent initialText

  noteContent <-
    if skipEditor
      then pure initialNoteContent
      else editor noteId noteInfo initialNoteContent

  updatedNoteInfo <- updateNoteInfo noteContent noteInfo

  if hasContent noteContent
    then do
      saveNote noteId updatedNoteInfo noteContent
      logMessage NoteCreated
    else do
      logMessage NoteEmpty

editNote :: (UI :> es, Database :> es, Files :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Bool -> Eff es ()
editNote noteFilter searchTerm skipPicker = do
  maybeSelectedNoteId <-
    if skipPicker
      then do
        preparedSearch <- prepareSearch noteFilter
        notes <- fmap fst <$> (preparedSearch ^. #searchNotes) searchTerm
        pure $ notes !? 0
      else pick noteFilter searchTerm
  case maybeSelectedNoteId of
    Just noteId -> do
      noteInfo <- getNoteInfo noteId
      noteContent <- readNote noteId noteInfo
      newNoteContent <- editor noteId noteInfo noteContent

      if hasContent newNoteContent
        then do
          updatedNoteInfo <- updateNoteInfo newNoteContent noteInfo
          saveNote noteId updatedNoteInfo newNoteContent
          logMessage NoteEdited
        else do
          logMessage NoteEmpty
    _ -> logMessage NoMatchingNote

deleteNote :: (UI :> es, Database :> es, Files :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Bool -> Eff es ()
deleteNote noteFilter searchTerm skipPicker = do
  maybeSelectedNoteId <-
    if skipPicker
      then do
        preparedSearch <- prepareSearch noteFilter
        notes <- fmap fst <$> (preparedSearch ^. #searchNotes) searchTerm
        pure $ notes !? 0
      else pick noteFilter searchTerm
  case maybeSelectedNoteId of
    Just noteId -> do
      answer <- prompt AreYouSureDeletion
      when answer $ do
        noteInfo <- getNoteInfo noteId
        fileDeleteNote noteId noteInfo
        dBDeleteNote noteId
    _ -> logMessage NoMatchingNote

viewNote :: (UI :> es, Database :> es, Files :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Bool -> Eff es ()
viewNote noteFilter searchTerm skipPicker = do
  maybeSelectedNoteId <-
    if skipPicker
      then do
        preparedSearch <- prepareSearch noteFilter
        notes <- fmap fst <$> (preparedSearch ^. #searchNotes) searchTerm
        pure $ notes !? 0
      else pick noteFilter searchTerm
  case maybeSelectedNoteId of
    Just noteId -> do
      noteInfo <- getNoteInfo noteId
      noteContent <- readNote noteId noteInfo
      displayNote noteId noteInfo noteContent
    _ -> logMessage NoMatchingNote

listNotes :: (UI :> es, Error NeoNoteError :> es, Database :> es, Files :> es) => NoteFilter -> Text -> [NoteAttribute] -> Int -> OrderBy NoteAttribute -> Eff es ()
listNotes noteFilter search noteAttributes showAmount orderBy = do
  displayNotes noteFilter search orderBy showAmount noteAttributes

scanNotes :: (IOE :> es) => Eff es ()
scanNotes = liftIO $ putStrLn "Sorry, not yet implemented"

updateNoteInfo :: (GetTime :> es) => NoteContent -> NoteInfo -> Eff es NoteInfo
updateNoteInfo noteContent noteInfo = do
  let tags = extractTags noteContent
  currentTime <- getCurrentTime
  pure $
    noteInfo
      { tags = tags,
        modified = currentTime
      }

makeNewNoteInfo :: (Database :> es, GetConfiguration :> es, GetTime :> es) => Eff es NoteInfo
makeNewNoteInfo = do
  currentTime <- getCurrentTime
  extension <- getConfiguration #noteExtension
  pure $
    NoteInfo
      { tags = mempty,
        extension = extension,
        created = currentTime,
        modified = currentTime
      }

makeNoteId :: (Database :> es, MakeId :> es) => Eff es NoteId
makeNoteId = do
  uncheckedId <- makeId
  idAlreadyUsed <- noteExists (NoteId uncheckedId)
  if not idAlreadyUsed
    then pure $ NoteId uncheckedId
    else makeNoteId

saveNote :: (Database :> es, Files :> es) => NoteId -> NoteInfo -> NoteContent -> Eff es ()
saveNote noteId noteInfo noteContent = do
  writeNote noteId noteInfo noteContent
  writeNoteInfo noteId noteInfo
