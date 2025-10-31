module NeoNote.CLI.Editor where

import Control.Exception (catch, finally)
import Control.Monad (guard, zipWithM_)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.List.NonEmpty
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T (readFile, writeFile)
import Effectful
import Effectful.Error.Dynamic
import GHC.Generics
import NeoNote.Configuration
import NeoNote.Error
import NeoNote.Log
import NeoNote.Note.Note
import Optics.Core
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.FSNotify
import System.FilePath (joinPath, takeBaseName, takeFileName, (</>))
import System.Process.Typed
import System.Random (randomRIO)

runEditor :: (Log :> es, IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => NonEmpty (EditHandle NoteInfo (Eff es)) -> Eff es ()
runEditor notes = do
  editorCommandTemplate <- getConfiguration #editor
  suffix <- pack . show <$> randomRIO @Int (10000, 99999)
  let fileName noteInfo = suffix <> "-" <> noteFileName noteInfo
  withRunInIO $ \unlift ->
    let files =
          notes
            <&> (#identifier %~ fileName)
            . (#edit %~ (fmap unlift .))
     in catch (runEditorTemporaryFile editorCommandTemplate files) $ \e ->
          unlift $ throwError $ EditingCrashed e

buildEditorCommand :: Text -> Text -> Text
buildEditorCommand editorCommandTemplate notePath =
  if T.elem '%' editorCommandTemplate
    then T.replace "%" notePath editorCommandTemplate
    else editorCommandTemplate <> " " <> notePath

data EditHandle a m = EditHandle
  { oldContent :: RawNote,
    edit :: IsEditorOpen -> RawNote -> m (),
    identifier :: a
  }
  deriving (Generic)

data IsEditorOpen = EditorOpen | EditorClosed deriving (Eq, Ord, Show)

runEditorTemporaryFile :: Text -> NonEmpty (EditHandle Text IO) -> IO ()
runEditorTemporaryFile editorCommandTemplate files = do
  withTemporaryFiles (files ^. mapping #identifier) $ \tmpDir filePaths -> do
    zipWithM_
      (\filePath oldContent -> T.writeFile filePath $ coerce oldContent)
      (toList filePaths)
      (toList $ files ^. mapping #oldContent)

    withManager $ \manager -> do
      let isNoteEvent = \case
            Modified {eventPath} -> eventPath `elem` filePaths
            _ -> False
          action = \case
            Modified {eventPath} -> do
              let filename = takeFileName eventPath
              case find (\eh -> eh ^. #identifier == T.pack filename) files of
                Just eh -> do
                  rawNote <- T.readFile (tmpDir </> filename)
                  (eh ^. #edit) EditorOpen (RawNote rawNote)
                Nothing -> pure ()
            _ -> pure ()

      stopListening <- watchDir manager tmpDir isNoteEvent action

      let cmd = shell $ unpack $ buildEditorCommand editorCommandTemplate (pack $ concat $ intersperse " " filePaths)
      exitCode <-
        runProcess cmd `finally` do
          newNoteContents <- traverse T.readFile filePaths
          zipWithM_
            ($)
            (fmap ($ EditorClosed) $ toList $ files ^. mapping #edit)
            (toList $ RawNote <$> newNoteContents)
      stopListening
      guard $ exitCode == ExitSuccess

withTemporaryFiles :: NonEmpty Text -> (FilePath -> NonEmpty FilePath -> IO a) -> IO a
withTemporaryFiles filenames handle = do
  tmpDirectory <- liftIO getTemporaryDirectory
  let tmpNoteDir = tmpDirectory </> "neonote"
  liftIO $ createDirectoryIfMissing True tmpNoteDir
  let filePaths = (\filename -> joinPath [tmpNoteDir, unpack filename]) <$> filenames
  mapM_ (`T.writeFile` "") filePaths
  finally (handle tmpNoteDir filePaths) (mapM_ removeFile filePaths)
