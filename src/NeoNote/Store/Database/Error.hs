module NeoNote.Store.Database.Error where

import Control.Exception (SomeException)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import GHC.Generics (Generic)
import NeoNote.Note.Note

data DatabaseError
  = MissingNoteId NoteId
  | CorruptedNoteId Text
  | InvalidDateFormat NoteId Text
  | TooManyResults NoteId
  | IncompatibleTableVersion Int Int
  | CorruptedTable Text
  | SQLiteDBCrashed SomeException
  deriving (Show, Generic)

prettyDatabaseError :: DatabaseError -> Text
prettyDatabaseError = \case
  MissingNoteId noteId -> [__i| I could not find the note #{noteId}, although it should be here |]
  CorruptedNoteId noteId -> [__i| #{noteId} is not a note id. Your note table has been corrupted |]
  InvalidDateFormat noteId date -> [__i| #{noteId} has an incorrect format for its creation date #{date} |]
  TooManyResults noteId -> [__i| There should only be one note with id #{noteId}, but there are more |]
  IncompatibleTableVersion current wanted -> [__i| Your note database version #{current} is not compatible with the wanted version #{wanted} |]
  CorruptedTable tableName -> [__i| The table #{tableName} is corrupted and does not match what I expected |]
  SQLiteDBCrashed err -> [__i| SQLite crashed due to #{err} |]
