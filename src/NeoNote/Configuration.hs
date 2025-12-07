module NeoNote.Configuration (Configuration (..), injectConfiguration, GetConfiguration (..), getConfiguration) where

import Control.Applicative (empty, many, (<|>))
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.Ini.Config
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as T (readFile, writeFile)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import NeoNote.Note.Note (AutoProperties (..), PropertyName)
import NeoNote.Note.Syntax.Common (makeFieldName)
import NeoNote.Note.Syntax.Pattern
import Optics.Core
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath (joinPath)

data Configuration f = Configuration
  { notesPath :: f FilePath,
    editor :: f Text,
    noteExtension :: f Text,
    autoProperties :: f AutoProperties
  }
  deriving (Generic)

data GetConfiguration :: Effect where
  GetConfiguration :: (forall f. Lens' (Configuration f) (f a)) -> GetConfiguration m a

makeEffect ''GetConfiguration

instance Semigroup (Configuration Maybe) where
  config1 <> config2 =
    Configuration
      { notesPath = config1 ^. #notesPath <|> config2 ^. #notesPath,
        editor = config1 ^. #editor <|> config2 ^. #editor,
        noteExtension = config1 ^. #noteExtension <|> config2 ^. #noteExtension,
        autoProperties = config1 ^. #autoProperties <> config2 ^. #autoProperties
      }

instance Monoid (Configuration Maybe) where
  mempty = Configuration Nothing Nothing Nothing Nothing

applyConfiguration :: Configuration Maybe -> Configuration Identity -> Configuration Identity
applyConfiguration config defConfig =
  Configuration
    { notesPath = maybe (defConfig ^. #notesPath) Identity (config ^. #notesPath),
      editor = maybe (defConfig ^. #editor) Identity (config ^. #editor),
      noteExtension = maybe (defConfig ^. #noteExtension) Identity (config ^. #noteExtension),
      autoProperties = maybe (defConfig ^. #autoProperties) Identity (config ^. #autoProperties)
    }

getDefaultConfiguration :: IO (Configuration Identity)
getDefaultConfiguration = do
  notesPath <- getXdgDirectory XdgData "neonote"
  editorEnv <- fmap pack <$> lookupEnv "EDITOR"
  let editor = fromMaybe "vim" editorEnv
  pure $
    Configuration
      { notesPath = Identity notesPath,
        editor = Identity editor,
        noteExtension = Identity "md",
        autoProperties = Identity (AutoProperties [])
      }

getEnvConfiguration :: IO (Configuration Maybe)
getEnvConfiguration = do
  notesPath <- lookupEnv "NEONOTE_PATH"
  noteExtension <- lookupEnv "NEONOTE_EXTENSION"
  editor <- fmap pack <$> lookupEnv "NEONOTE_EDITOR"
  pure $
    Configuration
      { notesPath = notesPath,
        editor = editor,
        noteExtension = pack <$> noteExtension,
        autoProperties = Nothing
      }

getFileConfiguration :: IO (Configuration Maybe)
getFileConfiguration = do
  configDir <- getXdgDirectory XdgConfig "neonote"
  let configPath = joinPath [configDir, "config.ini"]
  configExists <- doesFileExist configPath
  iniFile <-
    if configExists
      then T.readFile configPath
      else do
        createDirectoryIfMissing True configDir
        T.writeFile configPath initialINIFile
        pure initialINIFile
  let parsedConfig = parseIniFile iniFile $ do
        config1 <- section "DEFAULT" $ do
          editor <- fieldMbOf "editor" string
          notesPath <- fieldMbOf "notespath" string
          noteExtension <- fieldMbOf "extension" string
          pure $
            Configuration
              { editor = editor,
                notesPath = notesPath,
                noteExtension = noteExtension,
                autoProperties = Nothing
              }
        autoProperties <- section "AUTOPROPERTIES" $ many $ do
          -- propertyName <- fieldOf "name" (maybe (Left "Cannot parse field name") Right . makeFieldName)
          patternResult <- compilePattern <$> field "pattern"
          either (const empty) pure patternResult
        let config2 = mempty {autoProperties = Just $ AutoProperties $ autoProperties}

        pure $ config1 <> config2
  pure $ fromRight mempty parsedConfig
  where
    initialINIFile :: Text
    initialINIFile =
      [i|[DEFAULT]
;editor=vim
;notespath=/home/user/Documents/neonote ;absolute path needed here!
;extension=md
|]

injectConfiguration :: (IOE :> es) => Configuration Maybe -> Eff (GetConfiguration : es) a -> Eff es a
injectConfiguration additionalConfiguration app = do
  defaultConfiguration <- liftIO getDefaultConfiguration
  envConfiguration <- liftIO getEnvConfiguration
  fileConfiguration <- liftIO getFileConfiguration
  let config =
        applyConfiguration
          (additionalConfiguration <> envConfiguration <> fileConfiguration)
          defaultConfiguration
  liftIO $ createDirectoryIfMissing True $ runIdentity $ config ^. #notesPath
  interpret
    (\_ (GetConfiguration optic) -> pure $ runIdentity $ view optic config)
    app
