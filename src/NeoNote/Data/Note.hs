module NeoNote.Data.Note where

import Data.Set qualified as S
import GHC.Generics
import NeoNote.Data.Id
import NeoNote.Time
import Data.Text (Text)
import qualified Data.Text as T
import Optics.Core ((^.))
import Data.Coerce (coerce)

newtype NoteId = NoteId Id deriving (Generic, Show, Eq, Ord)

noteIdToText :: NoteId -> Text
noteIdToText (NoteId uuid) = idToText uuid

noteIdFromText :: Text -> Maybe NoteId
noteIdFromText text = NoteId <$> parseId text

data NoteFilter
  = HasTag Tag
  | Not NoteFilter
  | And NoteFilter NoteFilter
  | Or NoteFilter NoteFilter
  | EveryNote

newtype Tag = Tag Text deriving (Generic, Show, Eq, Ord)

data NoteInfo = NoteInfo
  { tags :: S.Set Tag,
    extension :: Text,
    createdAt :: Time,
    modifiedAt :: Time
  }
  deriving (Generic, Show, Eq, Ord)

newtype NoteContent = NoteContent Text deriving (Generic, Show, Eq, Ord, Semigroup, Monoid)

hasContent :: NoteContent -> Bool
hasContent (NoteContent content) =  not $
  T.null (T.strip content)

noteFileName :: NoteId -> NoteInfo -> Text
noteFileName noteId noteInfo = coerce noteId <> "." <> noteInfo ^. #extension