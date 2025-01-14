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
      { amount :: Int,
        search :: Text
      }
  | DeleteNote
      { amount :: Int,
        search :: Text
      }
  | ViewNote
      { amount :: Int,
        plain :: Bool,
        search :: Text
      }
  | PickNote
      { search :: Text
      }
  | ListNotes
      { showAttributes :: [NoteAttribute],
        showAmount :: Int,
        orderBy :: OrderBy NoteAttribute,
        search :: Text
      }
  | AddNotes {path :: [FilePath]}
  deriving (Generic, Show, Eq, Ord)
