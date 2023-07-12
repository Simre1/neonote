module NeoNote.UI where

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
import NeoNote.Time
import NeoNote.UI.Editor (runEditor)
import NeoNote.UI.ParseArguments (parseActionFromArguments)
import NeoNote.UI.Picker (picker)
import NeoNote.UI.Prompt
import NeoNote.UI.DisplayNotes
import NeoNote.UI.DisplayNote (displayNoteInTerminal)
import NeoNote.Store.Note (NoteStore)
import NeoNote.Search
import Data.List.NonEmpty

data UI :: Effect where
  GetActionFromArguments :: UI m Action
  Editor :: NonEmpty (NoteInfo,NoteContent) -> UI m (NonEmpty NoteContent)
  Pick :: NoteFilter -> Text -> UI m (Maybe NoteInfo)
  Prompt :: Prompt a -> UI m a
  DisplayNotes :: NoteFilter -> Text -> OrderBy NoteAttribute -> Int -> [NoteAttribute]  -> UI m ()
  DisplayNote :: NoteInfo -> NoteContent -> UI m ()

makeEffect ''UI

runUI :: (IOE :> es, NoteStore :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es, GetConfiguration :> es, NoteSearch :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ uiEffect -> do
  case uiEffect of
    Editor notes -> runEditor notes
    GetActionFromArguments -> getCurrentTime >>= liftIO . parseActionFromArguments
    Pick noteFilter searchTerm -> picker noteFilter searchTerm
    Prompt promptType -> askPrompt promptType
    DisplayNotes noteFilter search orderBy displayAmount noteAttributes -> displayNotesInTerminal noteFilter search orderBy displayAmount noteAttributes
    DisplayNote noteInfo noteContent -> displayNoteInTerminal noteInfo noteContent
