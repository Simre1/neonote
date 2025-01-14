module NeoNote.CLI where

import Data.List.NonEmpty
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic (Error)
import Effectful.TH (makeEffect)
import NeoNote.Actions (Action)
import NeoNote.CLI.DisplayNote (displayNoteInTerminal)
import NeoNote.CLI.DisplayNotes
import NeoNote.CLI.Editor (runEditor)
import NeoNote.CLI.ParseArguments (parseActionFromArguments)
import NeoNote.CLI.Picker (PickedAction, picker)
import NeoNote.CLI.Prompt
import NeoNote.Configuration
import NeoNote.Error (NeoNoteError)
import NeoNote.Log
import NeoNote.Note.Highlight (Highlight)
import NeoNote.Note.Note
import NeoNote.Store.Note (NoteStore)
import NeoNote.Time

data CLI :: Effect where
  GetActionFromArguments :: CLI m Action
  Editor :: NonEmpty Note -> CLI m (NonEmpty NoteContent)
  Pick :: NoteFilter -> Text -> (Maybe PickedAction -> m Bool) -> CLI m ()
  Prompt :: Prompt a -> CLI m a
  DisplayNotes :: NoteFilter -> OrderBy NoteAttribute -> Int -> [NoteAttribute] -> CLI m ()
  DisplayNote :: Bool -> Note -> CLI m ()

makeEffect ''CLI

runCLI :: (IOE :> es, Highlight :> es, NoteStore :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es, GetConfiguration :> es) => Eff (CLI : es) a -> Eff es a
runCLI = interpret $ \env uiEffect -> do
  case uiEffect of
    Editor notes -> runEditor notes
    GetActionFromArguments -> liftIO parseActionFromArguments
    Pick noteFilter searchTerm handlePickedAction -> localSeqUnlift env $ \unlift ->
      picker noteFilter searchTerm (unlift . handlePickedAction)
    Prompt promptType -> askPrompt promptType
    DisplayNotes noteFilter orderBy displayAmount noteAttributes ->
      displayNotesInTerminal noteFilter orderBy displayAmount noteAttributes
    DisplayNote plain note -> displayNoteInTerminal plain note
