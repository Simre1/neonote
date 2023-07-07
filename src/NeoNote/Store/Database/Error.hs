module NeoNote.Store.Database.Error where
import NeoNote.Data.Note
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Exception (SomeException)

data DatabaseError
  = MissingNoteId NoteId
  | CorruptedNoteId Text
  | InvalidDateFormat NoteId Text
  | TooManyResults NoteId
  | IncompatibleTableVersion Int Int
  | CorruptedTable Text
  | SQLiteDBCrashed SomeException
  deriving (Show, Generic)

