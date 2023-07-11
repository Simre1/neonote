module NeoNote.Store.Note where

import Effectful
import Effectful.Dispatch.Dynamic (LocalEnv, interpret, localSeqUnlift)
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Note.Note
import NeoNote.Note.Parse (extractTags)
import NeoNote.Store.Database
import NeoNote.Store.Files
import NeoNote.Time
import Optics.Core

data NoteStore :: Effect where
  NoteExists :: NoteId -> NoteStore m Bool
  FindNotes :: NoteFilter -> NoteStore m [NoteId]
  WriteNote :: NoteInfo -> NoteContent -> NoteStore m ()
  ReadNote :: NoteInfo -> NoteStore m NoteContent
  DeleteNote :: NoteInfo -> NoteStore m ()
  GetNoteInfo :: NoteId -> NoteStore m NoteInfo
  CreateNote :: (NoteInfo -> m (Maybe NoteContent)) -> NoteStore m ()

makeEffect ''NoteStore

runNoteStore :: (Database :> es, Files :> es, GetTime :> es, GetConfiguration :> es, MakeId :> es) => Eff (NoteStore : es) a -> Eff es a
runNoteStore = interpret $ \env -> \case
  NoteExists noteId -> runNoteExists noteId
  FindNotes noteFilter -> runFindNotes noteFilter
  WriteNote noteInfo noteContent -> runWriteNote noteInfo noteContent
  ReadNote noteInfo -> runReadNote noteInfo
  DeleteNote noteInfo -> runDeleteNote noteInfo
  GetNoteInfo noteId -> runGetNoteInfo noteId
  CreateNote f -> runCreateNote env f

runNoteExists :: (Database :> es, Files :> es) => NoteId -> Eff es Bool
runNoteExists = dbNoteExists

runFindNotes :: (Database :> es, Files :> es) => NoteFilter -> Eff es [NoteId]
runFindNotes = dbFindNotes

runWriteNote :: (Database :> es, Files :> es, GetTime :> es) => NoteInfo -> NoteContent -> Eff es ()
runWriteNote noteInfo noteContent = do
  let tags = extractTags noteContent
  currentTime <- getCurrentTime
  let updatedNoteInfo =
        noteInfo
          { tags = tags,
            modified = currentTime
          }
  filesWriteNote updatedNoteInfo noteContent
  dbWriteNoteInfo updatedNoteInfo

runReadNote :: (Database :> es, Files :> es) => NoteInfo -> Eff es NoteContent
runReadNote = filesReadNote

runDeleteNote :: (Database :> es, Files :> es) => NoteInfo -> Eff es ()
runDeleteNote noteInfo = do
  dbDeleteNote $ noteInfo ^. #id
  filesDeleteNote noteInfo

runGetNoteInfo :: (Database :> es, Files :> es) => NoteId -> Eff es NoteInfo
runGetNoteInfo = dbGetNoteInfo

runCreateNote :: (Database :> es, Files :> es, GetConfiguration :> es, MakeId :> es, GetTime :> es) => LocalEnv localEs es -> (NoteInfo -> Eff localEs (Maybe NoteContent)) -> Eff es ()
runCreateNote env getNoteContent = do
  noteInfo <- makeNewNoteInfo
  maybeNoteContent <- localSeqUnlift env $ \unlift -> unlift $ getNoteContent noteInfo
  maybe
    mempty
    ( \noteContent -> do
        filesWriteNote noteInfo noteContent
        dbWriteNoteInfo noteInfo
    )
    maybeNoteContent

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