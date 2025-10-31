{-# LANGUAGE MultiWayIf #-}

module NeoNote.Log where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Maybe
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.IO qualified as T (putStrLn)
import Effectful
import Effectful.Dispatch.Dynamic (impose_, interpret, localUnlift, reinterpret, reinterpret_)
import Effectful.Reader.Dynamic (ask, local, runReader)
import Effectful.State.Dynamic (modify, runStateShared)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import NeoNote.Note.Note
import NeoNote.Note.Syntax.RawNote (JunkField (..))

data NoteMessage
  = NoteCreated
  | NoteEmpty
  | NoteEdited
  | NoteUnchanged
  | NoteDeleted
  | FrontmatterJunk [JunkField]
  deriving (Generic, Show, Eq, Ord)

data Message
  = NoteMessage NoteId NoteMessage
  | NoMatchingNote
  | NotesAdded

data Log :: Effect where
  LogMessage :: Message -> Log m ()
  SkipLogging :: m a -> Log m a

makeEffect ''Log

logNoteMessage :: (Log :> es) => NoteId -> NoteMessage -> Eff es ()
logNoteMessage noteId noteMessage = logMessage $ NoteMessage noteId noteMessage

showMessage :: Message -> Text
showMessage NoMatchingNote = "No notes match your query"
showMessage NotesAdded = "Notes have been added"
showMessage (NoteMessage noteId NoteCreated) = [__i| Note #{noteIdToText noteId} has been created |]
showMessage (NoteMessage noteId NoteEmpty) = [__i| Note #{noteIdToText noteId} is empty, so nothing has been done |]
showMessage (NoteMessage noteId NoteEdited) = [__i| Note #{noteIdToText noteId} has been edited |]
showMessage (NoteMessage noteId NoteUnchanged) = [__i| Note #{noteIdToText noteId} is unchanged, so nothing has been done |]
showMessage (NoteMessage noteId NoteDeleted) = [__i| Note #{noteIdToText noteId} has been deleted |]
showMessage (NoteMessage noteId (FrontmatterJunk junkLines)) =
  let lines :: Text = mconcat (intersperse "\n" (coerce junkLines))
   in [__i| Your front matter for #{noteIdToText noteId} contained invalid lines:\n#{lines}} |]

runLog :: (IOE :> es) => Eff (Log : es) a -> Eff es a
runLog = reinterpret (runReader True) $ \env -> \case
  LogMessage message -> do
    shouldLog <- ask
    when shouldLog $ liftIO $ T.putStrLn (showMessage message)
  SkipLogging m -> localUnlift env (ConcUnlift Persistent Unlimited) $ \unlift ->
    local (\_ -> False) (unlift m)
