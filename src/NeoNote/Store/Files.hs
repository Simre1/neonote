module NeoNote.Store.Files () where

import Control.Exception (SomeException, catch)
import Data.Coerce (coerce)
import Data.IORef
import Data.Map qualified as M
import Data.Text (unpack)
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Error
import NeoNote.Note.Note
import Optics.Core
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (joinPath)

data Files :: Effect where
  FilesWriteNote :: NoteInfo -> NoteContent -> Files m ()
  FilesReadNote :: NoteInfo -> Files m NoteContent
  FilesDeleteNote :: NoteInfo -> Files m ()

makeEffect ''Files

runFiles :: (IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => Eff (Files : es) a -> Eff es a
runFiles eff = do
  notesPath <- getConfiguration #notesPath
  noteContentCache <- makeNoteContentCache
  interpret
    ( \_ filesEffect -> do
        withRunInIO $ \unlift -> catch
          ( case filesEffect of
              FilesWriteNote noteInfo noteContent ->
                handleWriteNote noteContentCache notesPath noteInfo noteContent
              FilesReadNote noteInfo ->
                handleReadNote noteContentCache notesPath noteInfo
              FilesDeleteNote noteInfo ->
                handleDeleteNote noteContentCache notesPath noteInfo
          )
          $ \e -> unlift $ throwError (FileAccessFailed e)
    )
    eff

notesDirectory :: FilePath -> FilePath
notesDirectory notesPath = joinPath [notesPath, "notes"]

handleWriteNote :: NoteContentCache -> FilePath -> NoteInfo -> NoteContent -> IO ()
handleWriteNote noteContentCache notesPath noteInfo noteContent = do
  let directory = notesDirectory notesPath
  createDirectoryIfMissing True directory
  cacheNoteContent noteContentCache (noteInfo ^. #id) noteContent
  T.writeFile (joinPath [directory, unpack $ noteIdToText (noteInfo ^. #id) <> "." <> noteInfo ^. #extension]) (coerce noteContent)

handleReadNote :: NoteContentCache -> FilePath -> NoteInfo -> IO NoteContent
handleReadNote noteContentCache notesPath noteInfo = do
  maybeNoteContent <- lookupCachedNoteContent noteContentCache (noteInfo ^. #id)
  case maybeNoteContent of
    Nothing -> do
      let directory = notesDirectory notesPath
          filePath = joinPath [directory, unpack $ noteIdToText (noteInfo ^. #id) <> "." <> noteInfo ^. #extension]
      catch
        ( do
            noteContent <- NoteContent <$> T.readFile filePath
            cacheNoteContent noteContentCache (noteInfo ^. #id) noteContent
            pure noteContent
        )
        $ \(_ :: SomeException) -> pure (NoteContent "Sorry, this file has been lost")
    Just noteContent -> pure noteContent

handleDeleteNote :: NoteContentCache -> FilePath -> NoteInfo -> IO ()
handleDeleteNote noteContentCache notesPath noteInfo = do
  let directory = notesDirectory notesPath
  removeCachedNoteContent noteContentCache (noteInfo ^. #id)
  removeFile (joinPath [directory, unpack $ noteIdToText (noteInfo ^. #id) <> "." <> noteInfo ^. #extension])

newtype NoteContentCache = NoteContentCache (IORef (M.Map NoteId NoteContent))

makeNoteContentCache :: (IOE :> es) => Eff es NoteContentCache
makeNoteContentCache = liftIO $ NoteContentCache <$> newIORef M.empty

lookupCachedNoteContent :: NoteContentCache -> NoteId -> IO (Maybe NoteContent)
lookupCachedNoteContent (NoteContentCache ref) noteId = do
  mapCache <- liftIO $ readIORef ref
  pure $ M.lookup noteId mapCache

cacheNoteContent :: NoteContentCache -> NoteId -> NoteContent -> IO ()
cacheNoteContent (NoteContentCache ref) noteId noteContent = liftIO $ modifyIORef' ref (M.insert noteId noteContent)

removeCachedNoteContent :: NoteContentCache -> NoteId -> IO ()
removeCachedNoteContent (NoteContentCache ref) noteId = liftIO $ modifyIORef' ref (M.delete noteId)
