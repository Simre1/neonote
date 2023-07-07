module NeoNote.Store.Files where

import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Data.Note
import Optics.Core
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

data Files :: Effect where
  GetDatabasePath :: Files m FilePath
  WriteNote :: NoteId -> NoteInfo -> NoteContent -> Files m ()
  ReadNote :: NoteId -> NoteInfo -> Files m NoteContent

makeEffect ''Files

runFiles :: (IOE :> es, GetConfiguration :> es) => Eff (Files : es) a -> Eff es a
runFiles = interpret $ \_ filesEffect -> do
  notesPath <- getConfiguration #notesPath
  case filesEffect of
    GetDatabasePath -> liftIO $ do
      createDirectoryIfMissing True notesPath
      pure $ databasePath notesPath
    WriteNote noteId noteInfo noteContent ->
      liftIO $
        handleWriteNote notesPath noteId noteContent (noteInfo ^. #extension)
    ReadNote noteId noteInfo ->
      liftIO $
        handleReadNote notesPath noteId (noteInfo ^. #extension)

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