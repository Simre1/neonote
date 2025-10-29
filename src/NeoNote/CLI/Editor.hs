module NeoNote.CLI.Editor where

import Control.Exception (catch, finally)
import Control.Monad (guard, zipWithM_)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.List.NonEmpty
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T (readFile, writeFile)
import Effectful
import Effectful.Error.Dynamic
import NeoNote.Configuration
import NeoNote.Error
import NeoNote.Log
import NeoNote.Note.Note
import Optics.Core
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath (joinPath)
import System.Process.Typed
import System.Random (randomRIO)

runEditor :: (Log :> es, IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => NonEmpty (NoteInfo, RawNote) -> Eff es (NonEmpty RawNote)
runEditor notes = do
  editorCommandTemplate <- getConfiguration #editor
  suffix <- pack . show <$> randomRIO @Int (10000, 99999)
  let fileName noteInfo = suffix <> "-" <> noteFileName noteInfo
  let files = first fileName <$> notes
  withRunInIO $ \unlift -> catch (runEditorTemporaryFile editorCommandTemplate files) $ \e -> do
    unlift $ throwError $ EditingCrashed e

buildEditorCommand :: Text -> Text -> Text
buildEditorCommand editorCommandTemplate notePath =
  if T.elem '%' editorCommandTemplate
    then T.replace "%" notePath editorCommandTemplate
    else editorCommandTemplate <> " " <> notePath

runEditorTemporaryFile :: Text -> NonEmpty (Text, RawNote) -> IO (NonEmpty RawNote)
runEditorTemporaryFile editorCommandTemplate files = do
  withTemporaryFiles (fst <$> files) $ \filePaths -> do
    zipWithM_ (\filePath oldNoteContent -> T.writeFile filePath $ coerce oldNoteContent) (toList filePaths) (snd <$> toList files)
    let cmd = shell $ unpack $ buildEditorCommand editorCommandTemplate (pack $ concat $ intersperse " " filePaths)
    exitCode <- runProcess cmd
    guard $ exitCode == ExitSuccess
    newNoteContents <- traverse T.readFile filePaths
    pure $ RawNote <$> newNoteContents

withTemporaryFiles :: NonEmpty Text -> (NonEmpty FilePath -> IO a) -> IO a
withTemporaryFiles filenames handle = do
  tmpDirectory <- liftIO getTemporaryDirectory
  let filePaths = (\filename -> joinPath [tmpDirectory, unpack filename]) <$> filenames
  mapM_ (`T.writeFile` "") filePaths
  finally (handle filePaths) (mapM_ removeFile filePaths)
