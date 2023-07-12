module NeoNote.Actions where

import Data.Text (Text)
import GHC.Generics (Generic)
import NeoNote.Note.Note

data Action
  = CreateNote
      { skipEditor :: Bool,
        noteContent :: Text
      }
  | EditNote
      { noteFilter :: NoteFilter,
        amount :: Int,
        search :: Text
      }
  | DeleteNote
      { noteFilter :: NoteFilter,
        amount :: Int,
        search :: Text
      }
  | ViewNote
      { noteFilter :: NoteFilter,
        amount :: Int,
        search :: Text
      }
  | PickNote
      { noteFilter :: NoteFilter,
        search :: Text
      }
  | ListNotes
      { noteFilter :: NoteFilter,
        showAttributes :: [NoteAttribute],
        showAmount :: Int,
        orderBy :: OrderBy NoteAttribute,
        search :: Text
      }
  | ScanNotes
  deriving (Generic, Show, Eq, Ord)
