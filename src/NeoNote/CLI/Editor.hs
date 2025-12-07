module NeoNote.CLI.Editor where

import Control.Exception (catch, finally)
import Control.Monad (guard, zipWithM_)
import Data.Coerce (coerce)
import Data.Foldable (find)
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
import NeoNote.Time
import Optics.Core
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.FSNotify
import System.FilePath (joinPath, takeFileName, (</>))
import System.Process.Typed
import System.Random (randomRIO)

runEditor :: (GetTime :> es, Log :> es, IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => (NoteId -> Time -> RawNote -> Eff es ()) -> NonEmpty (NoteInfo, RawNote) -> Eff es (NonEmpty RawNote)
runEditor stageEdit notes = do
  editorCommandTemplate <- getConfiguration #editor
  withRunInIO $ \unlift ->
    let stageEditIO = fmap (fmap unlift) . stageEdit
     in -- (#edit %~ (fmap unlift .))
        catch (runEditorTemporaryFile editorCommandTemplate stageEditIO (unlift getCurrentTime) notes) $ \e ->
          unlift $ throwError $ EditingCrashed e

buildEditorCommand :: Text -> Text -> Text
buildEditorCommand editorCommandTemplate notePath =
  if T.elem '%' editorCommandTemplate
    then T.replace "%" notePath editorCommandTemplate
    else editorCommandTemplate <> " " <> notePath

runEditorTemporaryFile :: Text -> (NoteId -> Time -> RawNote -> IO ()) -> IO Time -> NonEmpty (NoteInfo, RawNote) -> IO (NonEmpty RawNote)
runEditorTemporaryFile editorCommandTemplate stageEdit getTimeIO notes = do
  suffix <- pack . show <$> randomRIO @Int (10000, 99999)
  let fileName noteInfo = suffix <> "-" <> noteFileName noteInfo
  let fileNames = fileName . fst <$> notes

  withTemporaryFiles fileNames $ \tmpDir filePaths -> do
    zipWithM_
      (\filePath oldContent -> T.writeFile filePath $ coerce oldContent)
      (toList filePaths)
      (toList $ snd <$> notes)

    withManager $ \manager -> do
      let isNoteEvent = \case
            Modified {eventPath} -> eventPath `elem` filePaths
            _ -> False
          action = \case
            Modified {eventPath} -> do
              let filename = takeFileName eventPath
              case find (\(ni, _) -> fileName ni == T.pack filename) notes of
                Just (ni, _) -> do
                  rawNote <- T.readFile (tmpDir </> filename)
                  modificationTime <- getTimeIO
                  coerce stageEdit (ni ^. #id) modificationTime (RawNote rawNote)
                Nothing -> pure ()
            _ -> pure ()

      stopListening <- watchDir manager tmpDir isNoteEvent action

      let cmd = shell $ unpack $ buildEditorCommand editorCommandTemplate (pack $ concat $ intersperse " " filePaths)
      exitCode <- runProcess cmd
      stopListening
      guard $ exitCode == ExitSuccess
      newNoteContents <- traverse T.readFile filePaths
      pure $ RawNote <$> newNoteContents

withTemporaryFiles :: NonEmpty Text -> (FilePath -> NonEmpty FilePath -> IO a) -> IO a
withTemporaryFiles filenames handle = do
  tmpDirectory <- liftIO getTemporaryDirectory
  let tmpNoteDir = tmpDirectory </> "neonote"
  liftIO $ createDirectoryIfMissing True tmpNoteDir
  let filePaths = (\filename -> joinPath [tmpNoteDir, unpack filename]) <$> filenames
  mapM_ (`T.writeFile` "") filePaths
  finally (handle tmpNoteDir filePaths) (mapM_ removeFile filePaths)
