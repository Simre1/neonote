module NeoNote.Configuration (Configuration (..), injectConfiguration, GetConfiguration(..), getConfiguration) where

import Control.Applicative ((<|>))
import Data.Functor.Identity
import Data.Text (Text, pack)
import Effectful
import GHC.Generics (Generic)
import Optics.Core
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.Environment (lookupEnv)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (interpret)

data Configuration f = Configuration
  { notesPath :: f FilePath,
    editor :: f Text,
    noteExtension :: f Text
  }
  deriving (Generic)

data GetConfiguration :: Effect where
  GetConfiguration :: Lens' (Configuration Identity) (Identity a) -> GetConfiguration m a

makeEffect ''GetConfiguration

instance Semigroup (Configuration Maybe) where
  config1 <> config2 =
    Configuration
      { notesPath = config1 ^. #notesPath <|> config1 ^. #notesPath,
        editor = config1 ^. #editor <|> config2 ^. #editor,
        noteExtension = config1 ^. #noteExtension <|> config2 ^. #noteExtension
      }

instance Monoid (Configuration Maybe) where
  mempty = Configuration Nothing Nothing Nothing

applyConfiguration :: Configuration Maybe -> Configuration Identity -> Configuration Identity
applyConfiguration config defConfig =
  Configuration
    { notesPath = maybe (defConfig ^. #notesPath) Identity (config ^. #notesPath),
      editor = maybe (defConfig ^. #editor) Identity (config ^. #editor),
      noteExtension = maybe (defConfig ^. #noteExtension) Identity (config ^. #noteExtension)
    }

getDefaultConfiguration :: IO (Configuration Identity)
getDefaultConfiguration = do
  notesPath <- getXdgDirectory XdgData "neonote"
  let editor = "vim"
  pure $
    Configuration
      { notesPath = Identity notesPath,
        editor = Identity editor,
        noteExtension = Identity "md"
      }

getEnvConfiguration :: IO (Configuration Maybe)
getEnvConfiguration = do
  notesPath <- lookupEnv "NEONOTE_PATH"
  noteExtension <- lookupEnv "NEONOTE_EXTENSION"
  editor1 <- fmap pack <$> lookupEnv "NEONOTE_EDITOR"
  editor2 <- fmap pack <$> lookupEnv "EDITOR"
  pure $
    Configuration
      { notesPath = notesPath,
        editor = editor1 <|> editor2,
        noteExtension = pack <$> noteExtension
      }

injectConfiguration :: (IOE :> es) => Configuration Maybe -> Eff (GetConfiguration : es) a -> Eff es a
injectConfiguration additionalConfiguration app = do
  defaultConfiguration <- liftIO getDefaultConfiguration
  envConfiguration <- liftIO getEnvConfiguration
  let config =
        applyConfiguration
          (additionalConfiguration <> envConfiguration)
          defaultConfiguration
  interpret 
    (\_ (GetConfiguration optic) -> pure $ runIdentity $ view optic config)
    app
