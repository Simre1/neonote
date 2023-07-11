module FakeEnvironment where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.State.Static.Local
import Effectful.Writer.Dynamic (runWriterLocal, tell)
import GHC.Generics (Generic)
import NeoNote.Configuration (Configuration (..), GetConfiguration (..), injectConfiguration)
import NeoNote.Data.Id (runMakeId)
import NeoNote.Error
import NeoNote.Log
import NeoNote.Run
import NeoNote.Store.Database (runDatabase)
import NeoNote.Store.Files (runFiles)
import NeoNote.Store.Note (runNoteStore)
import NeoNote.Time (runGetTime)
import NeoNote.UI (runUI)
import Optics.Core
import System.Directory (doesDirectoryExist, getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath (joinPath)
import System.Random (randomRIO)

runFakeIO :: FakeData -> Eff AppEffects a -> IO (FakeOutput a)
runFakeIO fakeData eff = do
  (result, messages) <-
    runEff
      . runFakeConfiguration fakeData
      . runFakeLog
      . runFakeError
      . runFiles
      . runDatabase
      . runMakeId
      . runGetTime
      . runNoteStore
      . runUI
      $ eff
  pure $ FakeOutput messages result

data FakeData = FakeData
  { defaultExtension :: Text,
    editorWrites :: [Text]
  }
  deriving (Generic, Show, Eq, Ord)

data FakeOutput a = FakeOutput
  { logs :: [Message],
    output :: Either NeoNoteError a
  }
  deriving (Generic, Show)

data ConfigFakeFunctor a where
  JustGetField :: a -> ConfigFakeFunctor a
  InsertEditorCmd :: ConfigFakeFunctor Text

runFakeConfiguration :: (IOE :> es) => FakeData -> Eff (GetConfiguration : es) a -> Eff es a
runFakeConfiguration fakeData eff = do
  tmpDirectory <- liftIO getTemporaryDirectory
  let makeFakeFolderName = do
        name <- (\n -> joinPath [tmpDirectory, "neonote-test", n]) . show <$> randomRIO @Int (1000, 10000)
        alreadyExists <- doesDirectoryExist name
        if alreadyExists then makeFakeFolderName else pure name
  fakeFolderName <- liftIO makeFakeFolderName
  let config =
        Configuration
          { notesPath = JustGetField fakeFolderName,
            editor = InsertEditorCmd,
            noteExtension = JustGetField "md"
          }
  liftIO $ createDirectoryIfMissing True fakeFolderName
  reinterpret
    (evalState $ fakeData ^. #editorWrites)
    ( \_ -> \case
        GetConfiguration optic -> do
          case config ^. optic of
            JustGetField a -> pure a
            InsertEditorCmd -> do
              text <- head <$> get
              modify @[Text] tail
              pure $ "echo -n \"" <> text <> "\" >>%"
    )
    eff

runFakeError :: Eff (Error NeoNoteError : es) a -> Eff es (Either NeoNoteError a)
runFakeError = runErrorNoCallStack

runFakeLog :: Eff (Log : es) a -> Eff es (a, [Message])
runFakeLog = reinterpret runWriterLocal $ \_ -> \case
  LogMessage message -> tell [message]
  _ -> error "not needed yet"
