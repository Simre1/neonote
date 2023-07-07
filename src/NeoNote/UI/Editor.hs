module NeoNote.UI.Editor where

import Control.Monad (guard)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as T (readFile, writeFile)
import Effectful
import NeoNote.Configuration
import NeoNote.Data.Note
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath (joinPath)
import System.Process.Typed
import System.Random (randomRIO)
import Data.Coerce (coerce)
import Control.Exception (finally, catch)
import NeoNote.Log
import Effectful.Error.Dynamic
import NeoNote.Error

runEditor :: (Log :> es, IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => NoteId -> NoteInfo -> NoteContent -> Eff es NoteContent
runEditor noteId noteInfo oldNoteContent = do
  editorCommand <- getConfiguration #editor
  suffix <- pack . show <$> randomRIO @Int (10000, 99999)
  let fileName = suffix <> "-" <> noteFileName noteId noteInfo
  withRunInIO $ \unlift -> catch (runEditorTemporaryFile editorCommand fileName oldNoteContent) $ \e -> do
    unlift $ throwError $ EditingCrashed e

runEditorTemporaryFile :: Text -> Text -> NoteContent -> IO NoteContent
runEditorTemporaryFile editorCommand fileName oldNoteContent = do
  withTemporaryFile fileName $ \filepath -> do
    T.writeFile filepath $ coerce oldNoteContent
    let cmd = shell $ unpack $ editorCommand <> " " <> pack filepath
    exitCode <- runProcess cmd
    guard $ exitCode == ExitSuccess
    newNoteContent <- T.readFile filepath
    pure $ NoteContent newNoteContent

withTemporaryFile :: Text -> (FilePath -> IO a) -> IO a
withTemporaryFile filename handle = do
  tmpDirectory <- liftIO getTemporaryDirectory
  let filePath = joinPath [tmpDirectory, unpack filename]
  T.writeFile filePath ""
  finally (handle filePath) (removeFile filePath)
