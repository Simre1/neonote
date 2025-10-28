module NeoNote.Store.Note where

import Control.Monad
import Data.List (sortBy)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (LocalEnv, interpret, localSeqUnlift)
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Note.Parse (extractTags)
import NeoNote.Store.Database
import NeoNote.Time
import Optics.Core

data NoteStore :: Effect where
  NoteExists :: NoteId -> NoteStore m Bool
  FindNotes :: NoteFilter -> NoteStore m [NoteId]
  WriteNote :: NoteInfo -> NoteContent -> NoteStore m ()
  ReadNote :: NoteId -> NoteStore m Note
  DeleteNote :: NoteId -> NoteStore m ()
  ReadNoteInfo :: NoteId -> NoteStore m NoteInfo
  CreateNote :: (NoteInfo -> m NoteContent) -> NoteStore m ()
  BulkCreateNotes :: [(Text, NoteContent)] -> NoteStore m ()

makeEffect ''NoteStore

runNoteStore :: (Database :> es, GetTime :> es, GetConfiguration :> es, MakeId :> es, Log :> es) => Eff (NoteStore : es) a -> Eff es a
runNoteStore = interpret $ \env -> \case
  NoteExists noteId -> runNoteExists noteId
  FindNotes noteFilter -> runFindNotes noteFilter
  WriteNote noteInfo noteContent -> runWriteNote noteInfo noteContent
  ReadNote noteId -> runReadNote noteId
  DeleteNote noteId -> runDeleteNote noteId
  ReadNoteInfo noteId -> runGetNoteInfo noteId
  CreateNote f -> runCreateNote env f
  BulkCreateNotes noteData -> runBulkCreateNotes noteData

runNoteExists :: (Database :> es) => NoteId -> Eff es Bool
runNoteExists = dbNoteExists

runFindNotes :: (Database :> es) => NoteFilter -> Eff es [NoteId]
runFindNotes noteFilter = do
  noteIds <- dbFindNotes noteFilter
  noteInfos <- traverse dbReadNoteInfo noteIds
  pure $ sortBy (orderNote AttributeModified) noteInfos ^. mapping #id

runWriteNote :: (Log :> es, Database :> es, GetTime :> es) => NoteInfo -> NoteContent -> Eff es ()
runWriteNote noteInfo noteContent = do
  if hasContent noteContent
    then do
      oldNote <- dbReadNote (noteInfo ^. #id)
      if oldNote ^. #content /= noteContent
        then do
          runWriteNoteNoLogging noteInfo noteContent
          logMessage NoteEdited
        else logMessage NoteUnchanged
    else logMessage NoteEmpty

runWriteNoteNoLogging :: (GetTime :> es, Database :> es) => NoteInfo -> NoteContent -> Eff es ()
runWriteNoteNoLogging noteInfo noteContent = do
  let tags = extractTags noteContent
  currentTime <- getCurrentTime
  let updatedNoteInfo =
        noteInfo
          { tags = tags,
            modified = currentTime
          }
  dbWriteNote (Note updatedNoteInfo noteContent)

runReadNote :: (Database :> es) => NoteId -> Eff es Note
runReadNote = dbReadNote

runDeleteNote :: (Database :> es) => NoteId -> Eff es ()
runDeleteNote noteId = do
  dbDeleteNote noteId

runGetNoteInfo :: (Database :> es) => NoteId -> Eff es NoteInfo
runGetNoteInfo = dbReadNoteInfo

runCreateNote :: (Database :> es, GetConfiguration :> es, MakeId :> es, GetTime :> es, Log :> es) => LocalEnv localEs es -> (NoteInfo -> Eff localEs NoteContent) -> Eff es ()
runCreateNote env getNoteContent = do
  noteInfo <- makeNewNoteInfo
  noteContent <- localSeqUnlift env $ \unlift -> unlift $ getNoteContent noteInfo
  if hasContent noteContent
    then do
      runWriteNoteNoLogging noteInfo noteContent
      logMessage NoteCreated
    else do
      logMessage NoteEmpty

runBulkCreateNotes :: (GetTime :> es, Database :> es, MakeId :> es, Log :> es) => [(Text, NoteContent)] -> Eff es ()
runBulkCreateNotes noteData = do
  currentTime <- getCurrentTime
  notes <- forM noteData $ \(extension, noteContent) -> do
    noteId <- makeNoteId
    let tags = extractTags noteContent
        noteInfo =
          NoteInfo
            { id = noteId,
              tags = tags,
              extension = extension,
              created = currentTime,
              modified = currentTime
            }
    pure $ Note noteInfo noteContent
  dbWriteNotes notes
  logMessage NotesAdded

makeNewNoteInfo :: (GetConfiguration :> es, GetTime :> es, Database :> es, MakeId :> es) => Eff es NoteInfo
makeNewNoteInfo = do
  noteId <- makeNoteId
  currentTime <- getCurrentTime
  extension <- getConfiguration #noteExtension
  pure $
    NoteInfo
      { id = noteId,
        tags = mempty,
        extension = extension,
        created = currentTime,
        modified = currentTime
      }

makeNoteId :: (Database :> es, MakeId :> es) => Eff es NoteId
makeNoteId = do
  uncheckedId <- makeId
  idAlreadyUsed <- dbNoteExists (NoteId uncheckedId)
  if not idAlreadyUsed
    then pure $ NoteId uncheckedId
    else makeNoteId

readNotes :: (NoteStore :> es) => [NoteId] -> Eff es [Note]
readNotes = traverse readNote
