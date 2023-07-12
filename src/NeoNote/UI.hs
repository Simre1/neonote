module NeoNote.UI where

import Data.List.NonEmpty
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic (Error)
import Effectful.TH (makeEffect)
import NeoNote.Actions (Action)
import NeoNote.Configuration
import NeoNote.Error (NeoNoteError)
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Search
import NeoNote.Store.Note (NoteStore)
import NeoNote.Time
import NeoNote.UI.DisplayNote (displayNoteInTerminal)
import NeoNote.UI.DisplayNotes
import NeoNote.UI.Editor (runEditor)
import NeoNote.UI.ParseArguments (parseActionFromArguments)
import NeoNote.UI.Picker (PickedAction, picker)
import NeoNote.UI.Prompt

data UI :: Effect where
  GetActionFromArguments :: UI m Action
  Editor :: NonEmpty (NoteInfo, NoteContent) -> UI m (NonEmpty NoteContent)
  Pick :: NoteFilter -> Text -> (Maybe PickedAction -> m Bool) -> UI m ()
  Prompt :: Prompt a -> UI m a
  DisplayNotes :: NoteFilter -> Text -> OrderBy NoteAttribute -> Int -> [NoteAttribute] -> UI m ()
  DisplayNote :: NoteInfo -> NoteContent -> UI m ()

makeEffect ''UI

runUI :: (IOE :> es, NoteStore :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es, GetConfiguration :> es, NoteSearch :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \env uiEffect -> do
  case uiEffect of
    Editor notes -> runEditor notes
    GetActionFromArguments -> getCurrentTime >>= liftIO . parseActionFromArguments
    Pick noteFilter searchTerm handlePickedAction -> localSeqUnlift env $ \unlift ->
      picker noteFilter searchTerm (unlift . handlePickedAction)
    Prompt promptType -> askPrompt promptType
    DisplayNotes noteFilter search orderBy displayAmount noteAttributes ->
      displayNotesInTerminal noteFilter search orderBy displayAmount noteAttributes
    DisplayNote noteInfo noteContent -> displayNoteInTerminal noteInfo noteContent
