module NeoNote.Store.Note where

import Control.Monad
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localLiftUnlift)
import Effectful.TH (makeEffect)
import NeoNote.CLI.Editor (ShouldLog (..))
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Note.Syntax.RawNote (parseRawNote)
import NeoNote.Store.Database
import NeoNote.Time
import Optics.Core

data NoteStore :: Effect where
  NoteExists :: NoteId -> NoteStore m Bool
  FindNotes :: NoteFilter -> NoteStore m [NoteId]
  WriteNote :: Note -> RawNote -> NoteStore m ()
  ReadNote :: NoteId -> NoteStore m Note
  DeleteNote :: NoteId -> NoteStore m ()
  ReadNoteInfo :: NoteId -> NoteStore m NoteInfo
  CreateNote :: (NoteInfo -> (ShouldLog -> RawNote -> m ()) -> m ()) -> NoteStore m ()
  BulkCreateNotes :: [(Text, RawNote)] -> NoteStore m ()

makeEffect ''NoteStore

runNoteStore :: (Database :> es, GetTime :> es, GetConfiguration :> es, MakeId :> es, Log :> es) => Eff (NoteStore : es) a -> Eff es a
runNoteStore = interpret $ \env -> \case
  NoteExists noteId -> runNoteExists noteId
  FindNotes noteFilter -> runFindNotes noteFilter
  WriteNote noteInfo rawNote -> runWriteNote noteInfo rawNote
  ReadNote noteId -> runReadNote noteId
  DeleteNote noteId -> runDeleteNote noteId
  ReadNoteInfo noteId -> runGetNoteInfo noteId
  CreateNote kont -> do
    localLiftUnlift env (ConcUnlift Persistent Unlimited) $ \lift unlift ->
      runCreateNote (\noteInfo handleRawNote -> unlift $ kont noteInfo (fmap lift . handleRawNote))
  BulkCreateNotes noteData -> runBulkCreateNotes noteData

runNoteExists :: (Database :> es) => NoteId -> Eff es Bool
runNoteExists = dbNoteExists

runFindNotes :: (Database :> es) => NoteFilter -> Eff es [NoteId]
runFindNotes noteFilter = do
  noteIds <- dbFindNotes noteFilter
  noteInfos <- traverse dbReadNoteInfo noteIds
  pure $ sortBy (orderNote AttributeModified) noteInfos ^. mapping #id

runWriteNote :: (Log :> es, Database :> es, GetTime :> es) => Note -> RawNote -> Eff es ()
runWriteNote oldNote rawNote = do
  let oldNoteInfo = oldNote ^. #info
      noteId = (oldNoteInfo ^. #id)
  if hasContent rawNote
    then do
      let (_, fields, noteContent) = parseRawNote rawNote
      if oldNote ^. #content == noteContent && fields == oldNote ^. #info % #fields
        then logNoteMessage noteId NoteUnchanged
        else do
          logNoteMessage noteId NoteEdited
          writeRawNote oldNoteInfo rawNote
    else do
      logNoteMessage noteId NoteDeleted
      dbDeleteNote noteId

-- logMessage NoteEmpty

-- writeRawNote noteInfo noteContent
-- logMessage NoteEdited
-- pure ()

writeRawNote :: (GetTime :> es, Database :> es, Log :> es) => NoteInfo -> RawNote -> Eff es ()
writeRawNote noteInfo rawNote = do
  let (junk, fields, noteContent) = parseRawNote rawNote
  currentTime <- getCurrentTime
  let updatedNoteInfo =
        noteInfo
          { fields,
            modified = currentTime
          }
  when (not (Prelude.null junk)) $ logNoteMessage (noteInfo ^. #id) (FrontmatterJunk junk)
  dbWriteNote (Note updatedNoteInfo noteContent)

runReadNote :: (Database :> es) => NoteId -> Eff es Note
runReadNote = dbReadNote

runDeleteNote :: (Database :> es) => NoteId -> Eff es ()
runDeleteNote noteId = do
  dbDeleteNote noteId

runGetNoteInfo :: (Database :> es) => NoteId -> Eff es NoteInfo
runGetNoteInfo = dbReadNoteInfo

runCreateNote ::
  (Database :> es, GetConfiguration :> es, MakeId :> es, GetTime :> es, Log :> es) =>
  (NoteInfo -> (ShouldLog -> RawNote -> Eff es ()) -> Eff es ()) -> Eff es ()
runCreateNote createRawNote = do
  noteInfo <- makeNewNoteInfo
  createRawNote noteInfo $ \shouldLog rawNote -> do
    if hasContent rawNote
      then do
        writeRawNote noteInfo rawNote
        when (shouldLog == DoLog) $ logNoteMessage (noteInfo ^. #id) NoteCreated
      else do
        when (shouldLog == DoLog) $ logNoteMessage (noteInfo ^. #id) NoteEmpty
        dbDeleteNote (noteInfo ^. #id)

runBulkCreateNotes :: (GetTime :> es, Database :> es, MakeId :> es, Log :> es) => [(Text, RawNote)] -> Eff es ()
runBulkCreateNotes noteData = do
  currentTime <- getCurrentTime
  notes <- forM noteData $ \(extension, rawNote) -> do
    noteId <- makeNoteId
    let (junk, fields, noteContent) = parseRawNote rawNote
        noteInfo =
          NoteInfo
            { id = noteId,
              fields = fields,
              extension = extension,
              created = currentTime,
              modified = currentTime
            }
    when (not (Prelude.null junk)) $ logNoteMessage noteId (FrontmatterJunk junk)
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
        fields = Fields mempty,
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
