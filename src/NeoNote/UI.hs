module NeoNote.UI where

import Effectful
import NeoNote.Actions (Action)
import NeoNote.UI.Editor (runEditor)
import NeoNote.UI.ParseArguments (parseActionFromArguments)
import NeoNote.Note.Note
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import Effectful.Dispatch.Dynamic
import Data.Text
import NeoNote.UI.Search (fuzzySearch)
import NeoNote.Store.Files (Files)
import NeoNote.Store.Database (Database)
import NeoNote.Log
import NeoNote.Error (NeoNoteError)
import Effectful.Error.Dynamic (Error)
import NeoNote.Time

data UI :: Effect where
  GetActionFromArguments :: UI m Action
  Editor :: NoteId -> NoteInfo -> NoteContent -> UI m NoteContent
  Search :: NoteFilter -> Text -> UI m (Maybe NoteId)

makeEffect ''UI

runUI :: (IOE :> es, Database :> es, GetTime :> es, Error NeoNoteError :> es, Log :> es, Files :> es, GetConfiguration :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ uiEffect -> do
  case uiEffect of
    Editor noteId noteInfo noteContent -> runEditor noteId noteInfo noteContent
    GetActionFromArguments -> getCurrentTime >>= liftIO . parseActionFromArguments
    Search noteFilter searchTerm -> fuzzySearch noteFilter searchTerm
      
