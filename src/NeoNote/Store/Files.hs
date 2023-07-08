module NeoNote.Store.Files where

import Control.Exception (catch)
import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Note.Note
import NeoNote.Error
import Optics.Core
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

data Files :: Effect where
  GetDatabasePath :: Files m FilePath
  WriteNote :: NoteId -> NoteInfo -> NoteContent -> Files m ()
  ReadNote :: NoteId -> NoteInfo -> Files m NoteContent

makeEffect ''Files

runFiles :: (IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => Eff (Files : es) a -> Eff es a
runFiles = interpret $ \_ filesEffect -> do
  notesPath <- getConfiguration #notesPath
  withRunInIO $ \unlift -> catch
    ( case filesEffect of
        GetDatabasePath -> do
          createDirectoryIfMissing True notesPath
          pure $ databasePath notesPath
        WriteNote noteId noteInfo noteContent ->
          handleWriteNote notesPath noteId noteContent (noteInfo ^. #extension)
        ReadNote noteId noteInfo ->
          handleReadNote notesPath noteId (noteInfo ^. #extension)
    )
    $ \e -> unlift $ throwError (FileAccessFailed e)

notesDirectory :: FilePath -> FilePath
notesDirectory notesPath = joinPath [notesPath, "notes"]

databasePath :: FilePath -> FilePath
databasePath notesPath = joinPath [notesPath, "notes.db"]

handleWriteNote :: FilePath -> NoteId -> NoteContent -> Text -> IO ()
handleWriteNote notesPath noteId noteContent extension = do
  let directory = notesDirectory notesPath
  createDirectoryIfMissing True directory
  T.writeFile (joinPath [directory, unpack $ noteIdToText noteId <> "." <> extension]) (coerce noteContent)

handleReadNote :: FilePath -> NoteId -> Text -> IO NoteContent
handleReadNote notesPath noteId extension = do
  let directory = notesDirectory notesPath
  NoteContent <$> T.readFile (joinPath [directory, unpack $ noteIdToText noteId <> "." <> extension])