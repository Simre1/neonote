module NeoNote.Note.Note where

import Data.Coerce (coerce)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import NeoNote.Data.Id
import NeoNote.Time
import Optics.Core ((^.))

newtype NoteId = NoteId Id deriving (Generic, Show, Eq, Ord)

noteIdToText :: NoteId -> Text
noteIdToText (NoteId uuid) = idToText uuid

noteIdFromText :: Text -> Maybe NoteId
noteIdFromText text = NoteId <$> parseId text

data DateLiteral
  = NoteCreated
  | NoteModified
  | DateLiteral IncompleteTime
  deriving (Show, Eq, Ord, Generic)

data NoteFilter
  = HasTag Tag
  | EqualDate DateLiteral DateLiteral
  | AfterDate DateLiteral DateLiteral
  | BeforeDate DateLiteral DateLiteral
  | Not NoteFilter
  | And NoteFilter NoteFilter
  | Or NoteFilter NoteFilter
  | EveryNote
  deriving (Show, Eq, Ord, Generic)

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
hasContent (NoteContent content) =
  not $
    T.null (T.strip content)

noteFileName :: NoteId -> NoteInfo -> Text
noteFileName noteId noteInfo = coerce noteId <> "." <> noteInfo ^. #extension
