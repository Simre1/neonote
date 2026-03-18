module NeoNote.Note.Note where

import Data.Coerce (coerce)
import Data.Flag
import Data.List (intersperse, sortBy)
import Data.Map qualified as M
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
  | DateLiteralTime IncompleteTime
  deriving (Show, Eq, Ord, Generic)

data Comparison
  = Equal
  | Similar
  | Greater
  | Lesser
  | GreaterEqual
  | LesserEqual
  deriving (Eq, Ord, Show, Generic)

data NoteFilter
  = HasProperty PropertyName
  | Check Comparison Literal Literal
  | Not NoteFilter
  | Together NoteFilter NoteFilter
  | And NoteFilter NoteFilter
  | Or NoteFilter NoteFilter
  | Contains Text
  | EveryNote
  deriving (Show, Eq, Ord, Generic)

data Literal
  = PropertyLiteral PropertyName
  | DateLiteral DateLiteral
  | IdLiteral
  | StringLiteral Text
  | IntLiteral Int
  deriving (Eq, Ord, Show, Generic)

data Value
  = StringValue Text
  | IntValue Int
  | NoValue
  deriving (Eq, Ord, Show)

newtype PropertyName = PropertyName {name :: Text} deriving (Eq, Ord, Show, Generic)

newtype Properties = Properties {kv :: M.Map PropertyName (S.Set Value)} deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

propertyValues :: Properties -> [(PropertyName, Value)]
propertyValues (Properties properties) =
  foldMap (\(n, vs) -> (n,) <$> vs) (M.toList $ fmap S.toList properties)

data NoteInfo = NoteInfo
  { id :: NoteId,
    properties :: Properties,
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
  AttributeProperties -> compareField #properties
  where
    compareField :: (Ord a) => Lens' NoteInfo a -> Ordering
    compareField field = compare (info2 ^. field) (info1 ^. field)

concatProperties :: Properties -> Text
concatProperties properties = mconcat $ intersperse "," $ coerce $ M.keys (properties ^. #kv)

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
