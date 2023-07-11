module NeoNote.Store.Files where

import Control.Exception (catch, SomeException)
import Data.Coerce (coerce)
import Data.Text (unpack)
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Note.Note
import NeoNote.Error
import Optics.Core
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (joinPath)

data Files :: Effect where
  FilesWriteNote :: NoteInfo -> NoteContent -> Files m ()
  FilesReadNote :: NoteInfo -> Files m NoteContent
  FilesDeleteNote :: NoteInfo -> Files m ()

makeEffect ''Files

runFiles :: (IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => Eff (Files : es) a -> Eff es a
runFiles = interpret $ \_ filesEffect -> do
  notesPath <- getConfiguration #notesPath
  withRunInIO $ \unlift -> catch
    ( case filesEffect of
        FilesWriteNote noteInfo noteContent ->
          handleWriteNote notesPath noteInfo noteContent 
        FilesReadNote noteInfo ->
          handleReadNote notesPath noteInfo
        FilesDeleteNote noteInfo ->
          handleDeleteNote notesPath noteInfo
    )
    $ \e -> unlift $ throwError (FileAccessFailed e)

notesDirectory :: FilePath -> FilePath
notesDirectory notesPath = joinPath [notesPath, "notes"]

handleWriteNote :: FilePath -> NoteInfo -> NoteContent -> IO ()
handleWriteNote notesPath noteInfo noteContent  = do
  let directory = notesDirectory notesPath
  createDirectoryIfMissing True directory
  T.writeFile (joinPath [directory, unpack $ noteIdToText (noteInfo ^. #id) <> "." <> noteInfo ^. #extension]) (coerce noteContent)

handleReadNote :: FilePath -> NoteInfo -> IO NoteContent
handleReadNote notesPath noteInfo = do
  let directory = notesDirectory notesPath
      filePath = joinPath [directory, unpack $ noteIdToText (noteInfo ^. #id) <> "." <> noteInfo ^. #extension]
  catch (NoteContent <$> T.readFile  filePath) $ 
    \(_ :: SomeException) -> pure (NoteContent "Sorry, this file has been lost")

handleDeleteNote :: FilePath -> NoteInfo -> IO ()
handleDeleteNote notesPath noteInfo = do
  let directory = notesDirectory notesPath
  removeFile (joinPath [directory, unpack $ noteIdToText (noteInfo ^. #id) <> "." <> noteInfo ^. #extension]) 
