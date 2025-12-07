module NeoNote.Store.Note
  ( NoteStore,
    runNoteStore,
    noteExists,
    findNotes,
    writeNote,
    readNote,
    readNotes,
    deleteNote,
    readNoteInfo,
    createNote,
    bulkCreateNotes,
    stageEdit,
    commitStagedEdits,
    Selection (..),
  )
where

import Control.Monad
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localLiftUnlift)
import Effectful.Dispatch.Static
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Note.Operations
import NeoNote.Store.Database
import NeoNote.Time
import Optics.Core

data NoteStore :: Effect where
  NoteExists :: NoteId -> NoteStore m Bool
  FindNotes :: NoteFilter -> Selection -> NoteStore m [NoteId]
  WriteNote :: Note -> RawNote -> NoteStore m ()
  ReadNote :: NoteId -> NoteStore m Note
  DeleteNote :: NoteId -> NoteStore m ()
  ReadNoteInfo :: NoteId -> NoteStore m NoteInfo
  CreateNote :: (NoteInfo -> m RawNote) -> NoteStore m ()
  BulkCreateNotes :: [(Text, RawNote)] -> NoteStore m ()
  StageEdit :: ((NoteId -> Time -> RawNote -> m ()) -> m a) -> NoteStore m a
  CommitStagedEdits :: NoteStore m ()

makeEffect ''NoteStore

runNoteStore :: (Database :> es, GetTime :> es, GetConfiguration :> es, MakeId :> es, Log :> es) => Eff (NoteStore : es) a -> Eff es a
runNoteStore = interpret $ \env -> \case
  NoteExists noteId -> runNoteExists noteId
  FindNotes noteFilter selection -> runFindNotes noteFilter selection
  WriteNote noteInfo rawNote -> runWriteNote noteInfo rawNote
  ReadNote noteId -> runReadNote noteId
  DeleteNote noteId -> runDeleteNote noteId
  ReadNoteInfo noteId -> runGetNoteInfo noteId
  CreateNote kont -> do
    localLiftUnlift env (ConcUnlift Persistent Unlimited) $ \_ unlift ->
      runCreateNote (\noteInfo -> unlift $ kont noteInfo)
  BulkCreateNotes noteData -> runBulkCreateNotes noteData
  StageEdit kont -> localLiftUnlift env (ConcUnlift Persistent Unlimited) $ \lift unlift ->
    runStageEdit (\make -> unlift $ kont (\a b c -> lift $ make a b c))
  CommitStagedEdits -> runCommitStagedEdits

runStageEdit :: (Database :> es) => ((NoteId -> Time -> RawNote -> Eff es ()) -> Eff es a) -> Eff es a
runStageEdit = dbStageEdit

runCommitStagedEdits :: (Database :> es, GetConfiguration :> es, GetTime :> es, MakeId :> es, Log :> es) => Eff es ()
runCommitStagedEdits = dbCommitStagedEdits $ \noteId time rawNote -> do
  exists <- runNoteExists noteId
  note <-
    if exists
      then runReadNote noteId
      else do
        newNoteInfo <- makeNewNoteInfo
        pure $ Note (newNoteInfo {id = noteId}) (NoteContent "")

  autoProperties <- getConfiguration #autoProperties
  let update = updateNote autoProperties note time rawNote

  case update of
    Delete -> logNoteMessage noteId NoteDeleted
    NoChanges -> logNoteMessage noteId NoteUnchanged
    Update (_, junkLines) -> do
      case NE.nonEmpty junkLines of
        Nothing -> pure ()
        Just frontmatterJunk -> logNoteMessage (note ^. #info % #id) $ FrontmatterJunk frontmatterJunk
      logNoteMessage noteId NoteEdited

  pure (fst <$> update)

runNoteExists :: (Database :> es) => NoteId -> Eff es Bool
runNoteExists = dbNoteExists

runFindNotes :: (Database :> es) => NoteFilter -> Selection -> Eff es [NoteId]
runFindNotes noteFilter selection = do
  noteIds <- dbFindNotes noteFilter selection
  noteInfos <- traverse dbReadNoteInfo noteIds
  pure $ noteInfos ^. mapping #id

runWriteNote :: (Log :> es, Database :> es, GetTime :> es, GetConfiguration :> es) => Note -> RawNote -> Eff es ()
runWriteNote oldNote rawNote = do
  modificationTime <- getCurrentTime
  autoProperties <- getConfiguration #autoProperties
  let oldNoteInfo = oldNote ^. #info
      noteId = (oldNoteInfo ^. #id)
      update = updateNote autoProperties oldNote modificationTime rawNote
  case update of
    Delete -> do
      logNoteMessage noteId NoteDeleted
      dbDeleteNote noteId
    NoChanges -> do
      dbWriteNote oldNote
      logNoteMessage noteId NoteUnchanged
    Update (updatedNote, junkFields) -> do
      dbWriteNote updatedNote
      for_ (NE.nonEmpty junkFields) $
        logNoteMessage noteId . FrontmatterJunk
      logNoteMessage noteId NoteEdited

runReadNote :: (Database :> es) => NoteId -> Eff es Note
runReadNote = dbReadNote

runDeleteNote :: (Database :> es) => NoteId -> Eff es ()
runDeleteNote noteId = do
  dbDeleteNote noteId

runGetNoteInfo :: (Database :> es) => NoteId -> Eff es NoteInfo
runGetNoteInfo = dbReadNoteInfo

runCreateNote ::
  (Database :> es, GetConfiguration :> es, MakeId :> es, GetTime :> es, Log :> es) =>
  (NoteInfo -> Eff es RawNote) -> Eff es ()
runCreateNote createRawNote = do
  noteInfo <- makeNewNoteInfo
  rawNote <- createRawNote noteInfo
  autoProperties <- getConfiguration #autoProperties
  let update = updateNote autoProperties (Note noteInfo (NoteContent "")) (noteInfo ^. #modified) rawNote
      noteId = noteInfo ^. #id
  case update of
    Delete -> do
      logNoteMessage noteId NoteEmpty
    NoChanges -> do
      logNoteMessage noteId NoteEmpty
    Update (updatedNote, junkFields) -> do
      dbWriteNote updatedNote
      for_ (NE.nonEmpty junkFields) $
        logNoteMessage noteId . FrontmatterJunk
      logNoteMessage noteId NoteCreated

runBulkCreateNotes :: (GetTime :> es, Database :> es, MakeId :> es, Log :> es, GetConfiguration :> es) => [(Text, RawNote)] -> Eff es ()
runBulkCreateNotes noteData = do
  currentTime <- getCurrentTime
  autoProperties <- getConfiguration #autoProperties
  notes <- forM noteData $ \(extension, rawNote) -> do
    noteId <- makeNoteId
    let emptyNoteInfo =
          NoteInfo
            { id = noteId,
              properties = Properties M.empty,
              extension = extension,
              created = currentTime,
              modified = currentTime
            }
        emptyNote = Note emptyNoteInfo (NoteContent "")
        update = updateNote autoProperties emptyNote currentTime rawNote
    case update of
      Delete -> do
        logNoteMessage noteId NoteEmpty
        pure Nothing
      NoChanges -> do
        logNoteMessage noteId NoteEmpty
        pure Nothing
      Update (updatedNote, junkFields) -> do
        for_ (NE.nonEmpty junkFields) $
          logNoteMessage noteId . FrontmatterJunk
        logNoteMessage noteId NoteCreated
        pure $ Just updatedNote

  dbWriteNotes (catMaybes notes)
  logMessage NotesAdded

-- noteId <- makeNoteId
-- let (junk, properties, noteContent) = parseRawNote rawNote
--     noteInfo =
--       NoteInfo
--         { id = noteId,
--           properties = properties,
--           extension = extension,
--           created = currentTime,
--           modified = currentTime
--         }

-- case NE.nonEmpty junk of
--   Nothing -> pure ()
--   Just frontmatterJunk -> logNoteMessage (noteInfo ^. #id) (FrontmatterJunk frontmatterJunk)

-- pure $ Note noteInfo noteContent

makeNewNoteInfo :: (GetConfiguration :> es, GetTime :> es, Database :> es, MakeId :> es) => Eff es NoteInfo
makeNewNoteInfo = do
  noteId <- makeNoteId
  currentTime <- getCurrentTime
  extension <- getConfiguration #noteExtension
  pure $
    NoteInfo
      { id = noteId,
        properties = Properties mempty,
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
