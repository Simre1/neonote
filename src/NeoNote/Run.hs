module NeoNote.Run where

import Control.Monad (when)
import Effectful
import Effectful.Error.Dynamic
import NeoNote.Actions
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Data.Note
import NeoNote.Store.Database
import NeoNote.Store.Files
import NeoNote.Time
import NeoNote.UI
import NeoNote.Log
import NeoNote.Error

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
  CreateNote -> createNote
  SearchNote -> searchNote

createNote :: (Database :> es, GetConfiguration :> es, GetTime :> es, MakeId :> es, UI :> es, Files :> es) => Eff es ()
createNote = do
  noteId <- makeNoteId
  noteInfo <- makeNoteInfo
  let initialNoteContent = mempty

  noteContent <- editor noteId noteInfo initialNoteContent

  when (hasContent noteContent) $
    saveNote noteId noteInfo noteContent

searchNote :: (UI :> es, Database :> es, Files :> es) => Eff es ()
searchNote = do
  maybeSelectedNoteId <- search ""
  case maybeSelectedNoteId of
    Just noteId -> do
      noteInfo <- getNoteInfo noteId
      noteContent <- readNote noteId noteInfo
      newNoteContent <- editor noteId noteInfo noteContent
      writeNote noteId noteInfo newNoteContent
    _ -> pure ()

makeNoteInfo :: (Database :> es, GetConfiguration :> es, GetTime :> es) => Eff es NoteInfo
makeNoteInfo = do
  currentTime <- getCurrentTime
  extension <- getConfiguration #noteExtension
  pure $
    NoteInfo
      { tags = mempty,
        extension = extension,
        createdAt = currentTime,
        modifiedAt = currentTime
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
