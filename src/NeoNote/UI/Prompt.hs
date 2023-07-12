module NeoNote.UI.Prompt where

import Data.List.NonEmpty
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import NeoNote.Note.Note
import NeoNote.Time (timeToString)
import Optics.Core

data Prompt a where
  AreYouSureDeletion :: NonEmpty (NoteInfo, NoteContent) -> Prompt Bool

promptQuestion :: Prompt a -> Text
promptQuestion (AreYouSureDeletion notes) = [i|Do you really want to delete the following notes notes:\n#{foldMap notesText notes}|]
  where
    notesText :: (NoteInfo, NoteContent) -> Text
    notesText (noteInfo, noteContent) = [i|#{timeToString $ noteInfo ^. #modified}: #{noteContentPreview noteContent}\n|]

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
