module NeoNote.Actions where

import Effectful
import GHC.Generics (Generic)
import NeoNote.Store.Database
import NeoNote.Data.Id (MakeId, makeId)
import NeoNote.Data.Note
import NeoNote.Time
import NeoNote.Configuration

data Action = CreateNote | SearchNote deriving (Eq, Ord, Show, Generic)

