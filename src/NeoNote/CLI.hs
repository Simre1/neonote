module NeoNote.CLI
  ( CLI,
    getActionFromArguments,
    editor,
    pick,
    prompt,
    displayNotes,
    displayNote,
    getStandardInput,
    runCLI,
    PickerCallbacks (..),
    PickedAction (..),
    EditHandle (..),
    IsEditorOpen (..),
  )
where

import Data.Functor ((<&>))
import Data.List.NonEmpty
import Data.Text
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic (Error)
import Effectful.TH (makeEffect)
import NeoNote.Actions (Action)
import NeoNote.CLI.DisplayNote (displayNoteInTerminal)
import NeoNote.CLI.DisplayNotes
import NeoNote.CLI.Editor (EditHandle (..), IsEditorOpen (..), runEditor)
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
import Optics.Core ((%~), (^.))
import System.IO (hIsTerminalDevice, stdin)

data CLI :: Effect where
  GetActionFromArguments :: CLI m Action
  Editor :: NonEmpty (EditHandle NoteInfo m) -> CLI m ()
  Pick :: Text -> PickerCallbacks m -> CLI m ()
  Prompt :: Prompt a -> CLI m a
  DisplayNotes :: [NoteAttribute] -> [NoteInfo] -> CLI m ()
  DisplayNote :: Bool -> Bool -> Note -> CLI m ()
  GetStandardInput :: CLI m Text

makeEffect ''CLI

runCLI :: (IOE :> es, Highlight :> es, NoteStore :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es, GetConfiguration :> es) => Eff (CLI : es) a -> Eff es a
runCLI = interpret $ \env uiEffect -> do
  case uiEffect of
    Editor notes -> localUnlift env (ConcUnlift Persistent Unlimited) $ \unlift -> runEditor (notes <&> (#edit %~ (fmap unlift .)))
    GetActionFromArguments -> liftIO parseActionFromArguments
    Pick searchTerm callbacks -> localUnlift env (ConcUnlift Persistent Unlimited) $ \unlift ->
      picker searchTerm $
        PickerCallbacks
          { findNotes = unlift . (callbacks ^. #findNotes),
            getNoteContent = unlift . (callbacks ^. #getNoteContent),
            handlePickedAction = unlift . (callbacks ^. #handlePickedAction)
          }
    Prompt promptType -> askPrompt promptType
    DisplayNotes noteAttributes notes ->
      displayNotesInTerminal noteAttributes notes
    DisplayNote plain frontmatter note -> displayNoteInTerminal plain frontmatter note
    GetStandardInput -> liftIO $ do
      isTerminal <- hIsTerminalDevice stdin
      if isTerminal
        then pure ""
        else T.getContents
