module NeoNote.UI.Prompt where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Data.String.Interpolate (i)

data Prompt a where
  AreYouSureDeletion :: Int -> Prompt Bool

promptQuestion :: Prompt a -> Text
promptQuestion (AreYouSureDeletion noteAmount) = [i|Do you really want to delete #{noteAmount} note/s?|]

askConfirmation :: (IOE :> es) => Eff es Bool
askConfirmation = do
  liftIO $ T.putStrLn "yes or no (y/n): "
  answer <- liftIO $ T.toLower <$> T.getLine
  if answer == "yes" || answer == "y"
    then pure True
    else
      if answer == "no" || answer == "n"
        then pure False
        else askConfirmation

askPrompt :: (IOE :> es) => Prompt a -> Eff es a
askPrompt prompt = do
  liftIO $ T.putStrLn $ promptQuestion prompt
  case prompt of
    AreYouSureDeletion _ -> askConfirmation
