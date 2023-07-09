module NeoNote.Actions where

import Data.Text (Text)
import GHC.Generics (Generic)
import NeoNote.Note.Note

data OrderBy a = Descending a | Ascending a deriving (Show, Eq, Ord, Generic)

data Action
  = CreateNote
      { skipEditor :: Bool,
        noteContent :: Text
      }
  | EditNote
      { noteFilter :: NoteFilter,
        skipPicker :: Bool,
        search :: Text
      }
  | DeleteNote
      { noteFilter :: NoteFilter,
        skipPicker :: Bool,
        search :: Text
      }
  | ViewNote
      { noteFilter :: NoteFilter,
        skipPicker :: Bool,
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
