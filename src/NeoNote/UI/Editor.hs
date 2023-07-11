module NeoNote.UI.Editor where

import Control.Monad (guard)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as T (readFile, writeFile)
import Data.Text qualified as T
import Effectful
import NeoNote.Configuration
import NeoNote.Note.Note
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath (joinPath)
import System.Process.Typed
import System.Random (randomRIO)
import Data.Coerce (coerce)
import Control.Exception (finally, catch)
import NeoNote.Log
import Effectful.Error.Dynamic
import NeoNote.Error

runEditor :: (Log :> es, IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => NoteInfo -> NoteContent -> Eff es NoteContent
runEditor noteInfo oldNoteContent = do
  editorCommandTemplate <- getConfiguration #editor
  suffix <- pack . show <$> randomRIO @Int (10000, 99999)
  let fileName = suffix <> "-" <> noteFileName noteInfo
  withRunInIO $ \unlift -> catch (runEditorTemporaryFile editorCommandTemplate fileName oldNoteContent) $ \e -> do
    unlift $ throwError $ EditingCrashed e

buildEditorCommand :: Text -> Text -> Text
buildEditorCommand editorCommandTemplate notePath = 
  if T.elem '%' editorCommandTemplate
    then T.replace "%" notePath editorCommandTemplate
    else editorCommandTemplate <> " " <> notePath

runEditorTemporaryFile :: Text -> Text -> NoteContent -> IO NoteContent
runEditorTemporaryFile editorCommandTemplate fileName oldNoteContent = do
  withTemporaryFile fileName $ \filePath -> do
    T.writeFile filePath $ coerce oldNoteContent
    let cmd = shell $ unpack $ buildEditorCommand editorCommandTemplate (pack filePath)
    exitCode <- runProcess cmd
    guard $ exitCode == ExitSuccess
    newNoteContent <- T.readFile filePath
    pure $ NoteContent newNoteContent

withTemporaryFile :: Text -> (FilePath -> IO a) -> IO a
withTemporaryFile filename handle = do
  tmpDirectory <- liftIO getTemporaryDirectory
  let filePath = joinPath [tmpDirectory, unpack filename]
  T.writeFile filePath ""
  finally (handle filePath) (removeFile filePath)
