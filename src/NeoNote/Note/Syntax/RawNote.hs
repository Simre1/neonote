module NeoNote.Note.Syntax.RawNote where

import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Bifunctor (Bifunctor (second))
import Data.Coerce
import Data.Either (partitionEithers)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Debug.Trace
import NeoNote.Note.Note
import NeoNote.Note.Syntax.Common
import NeoNote.Note.Syntax.Pattern
import Optics.Core
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

newtype JunkField = JunkField Text deriving (Eq, Ord, Show)

parseRawNote :: RawNote -> ([JunkField], Properties, NoteContent)
parseRawNote (RawNote rawText) =
  fromMaybe (error "Parsing of raw note failed. This should not be possible") $
    flip P.parseMaybe rawText $ do
      whitespaceP
      (junk, frontProps) <- frontmatter
      noteContent <- content
      pure (junk, frontProps, noteContent)
  where
    frontmatter :: Parser ([JunkField], Properties)
    frontmatter = (<|> pure ([], Properties M.empty)) $ P.try $ do
      delimiterLine
      fieldLines <- P.many $ do
        line <- Right <$> fieldP <|> Left <$> junkFieldP
        pure line
      let (junk, fields) = partitionEithers fieldLines

      delimiterLine

      pure (junk, Properties $ M.fromList $ second (S.fromList) <$> fields)

    delimiterLine :: Parser ()
    delimiterLine = lexeme $ do
      line <- P.takeWhileP (Just "Delimiter line") (== '-')
      guard $ T.length line >= 3
      lineWhitespaceP
      void P.newline <|> P.eof
      whitespaceP

    junkFieldP :: Parser JunkField
    junkFieldP = do
      P.notFollowedBy delimiterLine
      txt <- P.takeWhile1P (Just "Junk field") (\c -> c /= ';' && c /= '\n')
      fieldSeparatorP
      pure $ JunkField txt

    fieldP :: Parser (PropertyName, [Value])
    fieldP = lexeme $ P.try $ do
      fn <- fieldNameP
      values <-
        P.choice
          [ do
              _ <- lexemeLine $ P.char ':'
              P.sepBy1
                (lexemeLine valueP)
                (lexemeLine $ P.char ','),
            pure [NoValue]
          ]
      lineWhitespaceP
      fieldSeparatorP

      pure (fn, values)

    fieldSeparatorP :: Parser ()
    fieldSeparatorP = lexeme $ void P.newline <|> void (P.char ';')

    content :: Parser NoteContent
    content = NoteContent <$> P.takeRest

extractAutoProperties :: AutoProperties -> NoteContent -> Properties
extractAutoProperties (AutoProperties patterns) (NoteContent content) =
  let props = (\(name, pattern) -> (name, matchPattern pattern content)) <$> M.toList patterns
      propsMap =
        foldl'
          ( \m (propertyName, values) ->
              M.alter
                ( \case
                    Nothing -> Just $ S.fromList values
                    Just values2 -> Just $ S.fromList values <> values2
                )
                propertyName
                m
          )
          M.empty
          props
   in Properties propsMap

makeFrontmatter :: Properties -> B.Builder
makeFrontmatter (Properties fields) =
  let fieldText = foldMap ((<> B.fromText "\n") . uncurry makeField) $ fmap (second S.toAscList) $ M.toList fields
      frontmatter = if M.size fields == 0 then "" else "---\n" <> fieldText <> "---\n\n"
   in frontmatter
  where
    makeField field [] = B.fromText (field ^. #name)
    makeField field xs = B.fromText (field ^. #name) <> ": " <> mconcat (intersperse ", " $ makeValue <$> xs)
    makeValue (StringValue txt) = B.fromText txt
    makeValue (IntValue n) = B.fromString (show n)

makeRawNote :: Properties -> NoteContent -> RawNote
makeRawNote fields noteContent =
  RawNote $
    TL.toStrict $
      B.toLazyText $
        makeFrontmatter fields <> B.fromText (noteContent ^. coerced)

noteToRaw :: Note -> RawNote
noteToRaw note =
  makeRawNote
    (note ^. #info % #properties)
    (note ^. #content)

-- noteFromRaw :: NoteInfo -> RawNote -> ([JunkField], Note)
-- noteFromRaw noteInfo rawNote =
--   let (junk, properties, content) = parseRawNote rawNote
--    in (junk, Note (noteInfo & #properties .~ properties) content)
