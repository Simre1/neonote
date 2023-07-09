module NeoNote.Actions where

import Data.Text (Text)
import GHC.Generics (Generic)
import NeoNote.Note.Note

data OrderBy a = Descending a | Ascending a deriving (Show, Eq, Ord, Generic)

data Action
  = CreateNote
      { noteContent :: Text,
        skipEditor :: Bool
      }
  | EditNote
      { noteFilter :: NoteFilter,
        search :: Text,
        skipPicker :: Bool
      }
  | DeleteNote
      { noteFilter :: NoteFilter,
        search :: Text,
        skipPicker :: Bool
      }
  | ViewNote
      { noteFilter :: NoteFilter,
        search :: Text,
        skipPicker :: Bool
      }
  | ListNotes
      { noteFilter :: NoteFilter,
        search :: Text,
        showAttributes :: [NoteAttribute],
        showAmount :: Int,
        orderBy :: OrderBy NoteAttribute 
      }
  | ScanNotes
  deriving (Generic, Show, Eq, Ord)
