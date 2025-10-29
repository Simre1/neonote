module NeoNote.Note.Note where

import Data.Coerce (coerce)
import Data.List (intersperse, sortBy)
import Data.Map qualified as M
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
  | DateLiteralTime IncompleteTime
  deriving (Show, Eq, Ord, Generic)

data Comparison
  = Equal
  | Greater
  | Lesser
  | GreaterEqual
  | LesserEqual
  deriving (Eq, Ord, Show, Generic)

data NoteFilter
  = HasField FieldName
  | Check Comparison Literal Literal
  | Not NoteFilter
  | Together NoteFilter NoteFilter
  | And NoteFilter NoteFilter
  | Or NoteFilter NoteFilter
  | Contains Text
  | EveryNote
  deriving (Show, Eq, Ord, Generic)

data Literal
  = FieldLiteral FieldName
  | DateLiteral DateLiteral
  | IdLiteral
  | StringLiteral Text
  | IntLiteral Int
  deriving (Eq, Ord, Show, Generic)

data Value
  = NoValue
  | StringValue Text
  | IntValue Int
  deriving (Eq, Ord, Show)

newtype FieldName = FieldName {name :: Text} deriving (Eq, Ord, Show, Generic)

newtype Fields = Fields {kv :: M.Map FieldName Value} deriving (Eq, Ord, Show, Generic)

data NoteInfo = NoteInfo
  { id :: NoteId,
    fields :: Fields,
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

newtype RawNote = RawNote Text deriving (Generic, Show, Eq, Ord, Semigroup, Monoid)

data NoteAttribute
  = AttributeId
  | AttributeCreated
  | AttributeModified
  | AttributeExtension
  | AttributeProperties
  deriving (Show, Eq, Generic, Ord)

hasContent :: RawNote -> Bool
hasContent (RawNote content) =
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
  AttributeProperties -> compareField #fields
  where
    compareField :: (Ord a) => Lens' NoteInfo a -> Ordering
    compareField field = compare (info2 ^. field) (info1 ^. field)

concatFields :: Fields -> Text
concatFields fields = mconcat $ intersperse "," $ coerce $ M.keys (fields ^. #kv)

data OrderBy a = Descending a | Ascending a deriving (Show, Eq, Ord, Generic)

newtype Ordered a = Ordered {list :: [a]} deriving (Eq, Ord, Show, Generic)

orderNotes :: OrderBy NoteAttribute -> Int -> [NoteInfo] -> Ordered NoteInfo
orderNotes ordering amount notes =
  let (applyOrder, orderNoteAttribute) = case ordering of
        Ascending a -> (reverse, a)
        Descending a -> (id, a)
   in Ordered $
        applyOrder $
          take amount $
            sortBy (\n1 n2 -> orderNote orderNoteAttribute n1 n2) notes
