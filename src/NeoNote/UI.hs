module NeoNote.UI where

import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic (Error)
import Effectful.TH (makeEffect)
import NeoNote.Actions (Action, OrderBy)
import NeoNote.Configuration
import NeoNote.Error (NeoNoteError)
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Store.Database (Database)
import NeoNote.Store.Files (Files)
import NeoNote.Time
import NeoNote.UI.Editor (runEditor)
import NeoNote.UI.ParseArguments (parseActionFromArguments)
import NeoNote.UI.Picker (picker)
import NeoNote.UI.Prompt
import NeoNote.UI.DisplayNotes
import NeoNote.UI.DisplayNote (displayNoteInTerminal)

data UI :: Effect where
  GetActionFromArguments :: UI m Action
  Editor :: NoteId -> NoteInfo -> NoteContent -> UI m NoteContent
  Pick :: NoteFilter -> Text -> UI m (Maybe NoteId)
  Prompt :: Prompt a -> UI m a
  DisplayNotes :: OrderBy NoteAttribute -> Int -> [NoteAttribute] -> [(NoteId, NoteInfo)] -> UI m ()
  DisplayNote :: NoteId -> NoteInfo -> NoteContent -> UI m ()

makeEffect ''UI

runUI :: (IOE :> es, Database :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es, Files :> es, GetConfiguration :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ uiEffect -> do
  case uiEffect of
    Editor noteId noteInfo noteContent -> runEditor noteId noteInfo noteContent
    GetActionFromArguments -> getCurrentTime >>= liftIO . parseActionFromArguments
    Pick noteFilter searchTerm -> picker noteFilter searchTerm
    Prompt promptType -> askPrompt promptType
    DisplayNotes orderBy displayAmount noteAttributes notes -> displayNotesInTerminal orderBy displayAmount noteAttributes notes
    DisplayNote noteId noteInfo noteContent -> displayNoteInTerminal noteId noteInfo noteContent
