module NeoNote.Note.Note where

import Data.Coerce (coerce)
import Data.List (intersperse)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import NeoNote.Data.Id
import NeoNote.Time
import Optics.Core (Lens', coerced, (%), (^.))

newtype NoteId = NoteId Id deriving (Generic, Show, Eq, Ord)

noteIdToText :: NoteId -> Text
noteIdToText (NoteId uuid) = idToText uuid

noteIdFromText :: Text -> Maybe NoteId
noteIdFromText text = NoteId <$> parseId text

data DateLiteral
  = DateLiteralCreated
  | DateLiteralModified
  | DateLiteral IncompleteTime
  deriving (Show, Eq, Ord, Generic)

data NoteFilter
  = HasTag Tag
  | EqualDate DateLiteral DateLiteral
  | AfterDate DateLiteral DateLiteral
  | BeforeDate DateLiteral DateLiteral
  | Not NoteFilter
  | And NoteFilter NoteFilter
  | Together NoteFilter NoteFilter
  | Or NoteFilter NoteFilter
  | Contains Text
  | EveryNote
  deriving (Show, Eq, Ord, Generic)

newtype Tag = Tag Text deriving (Generic, Show, Eq, Ord)

data NoteInfo = NoteInfo
  { id :: NoteId,
    tags :: S.Set Tag,
    extension :: Text,
    created :: Time,
    modified :: Time
  }
  deriving (Generic, Eq, Ord, Show)

data Note = Note
  { info :: NoteInfo,
    content :: NoteContent
  }
  deriving (Generic, Eq, Ord, Show)

newtype NoteContent = NoteContent Text deriving (Generic, Show, Eq, Ord, Semigroup, Monoid)

data NoteAttribute
  = AttributeId
  | AttributeCreated
  | AttributeModified
  | AttributeExtension
  | AttributeTags
  deriving (Show, Eq, Generic, Ord)

hasContent :: NoteContent -> Bool
hasContent (NoteContent content) =
  not $
    T.null (T.strip content)

noteContentPreview :: NoteContent -> Text
noteContentPreview (NoteContent noteContent) = T.replace "\n" "  " $ T.take 40 noteContent

noteFileName :: NoteInfo -> Text
noteFileName noteInfo = noteInfo ^. #id % coerced <> "." <> noteInfo ^. #extension % coerced

orderNote :: NoteAttribute -> NoteInfo -> NoteInfo -> Ordering
orderNote noteAttribute info1 info2 = case noteAttribute of
  AttributeId -> compareField #id
  AttributeCreated -> compareField #created
  AttributeModified -> compareField #modified
  AttributeExtension -> compareField #extension
  AttributeTags -> compareField #tags
  where
    compareField :: (Ord a) => Lens' NoteInfo a -> Ordering
    compareField field = compare (info2 ^. field) (info1 ^. field)

concatTags :: S.Set Tag -> Text
concatTags tags =
  mconcat $
    intersperse "," $
      coerce
        <$> S.toList tags

data OrderBy a = Descending a | Ascending a deriving (Show, Eq, Ord, Generic)
