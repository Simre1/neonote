module NeoNote.CLI
  ( CLI,
    getActionFromArguments,
    editor,
    pick,
    prompt,
    displayNotes,
    displayNote,
    runCLI,
    PickerCallbacks (..),
    PickedAction (..),
  )
where

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
import NeoNote.CLI.Picker (PickedAction (..), PickerCallbacks (..), picker)
import NeoNote.CLI.Prompt
import NeoNote.Configuration
import NeoNote.Error (NeoNoteError)
import NeoNote.Log
import NeoNote.Note.Highlight (Highlight)
import NeoNote.Note.Note
import NeoNote.Store.Note (NoteStore)
import NeoNote.Time
import Optics.Core ((^.))

data CLI :: Effect where
  GetActionFromArguments :: CLI m Action
  Editor :: NonEmpty (NoteInfo, RawNote) -> CLI m (NonEmpty RawNote)
  Pick :: Text -> PickerCallbacks m -> CLI m ()
  Prompt :: Prompt a -> CLI m a
  DisplayNotes :: [NoteAttribute] -> Ordered NoteInfo -> CLI m ()
  DisplayNote :: Bool -> Note -> CLI m ()

makeEffect ''CLI

runCLI :: (IOE :> es, Highlight :> es, NoteStore :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es, GetConfiguration :> es) => Eff (CLI : es) a -> Eff es a
runCLI = interpret $ \env uiEffect -> do
  case uiEffect of
    Editor notes -> runEditor notes
    GetActionFromArguments -> liftIO parseActionFromArguments
    Pick searchTerm callbacks -> localSeqUnlift env $ \unlift ->
      picker searchTerm $
        PickerCallbacks
          { findNotes = unlift . (callbacks ^. #findNotes),
            getNoteContent = unlift . (callbacks ^. #getNoteContent),
            handlePickedAction = unlift . (callbacks ^. #handlePickedAction)
          }
    Prompt promptType -> askPrompt promptType
    DisplayNotes noteAttributes notes ->
      displayNotesInTerminal noteAttributes notes
    DisplayNote plain note -> displayNoteInTerminal plain note
