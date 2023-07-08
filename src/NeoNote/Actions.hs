module NeoNote.Actions where

import GHC.Generics (Generic)
import NeoNote.Note.Note

data Action
  = CreateNote
  | SearchNote NoteFilter
  deriving (Generic)
