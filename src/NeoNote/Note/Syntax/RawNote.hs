{-# LANGUAGE ViewPatterns #-}

module NeoNote.Note.Syntax.RawNote where

import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Coerce
import Data.Either (partitionEithers)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import NeoNote.Note.Note
import NeoNote.Note.Syntax.Common
import Optics.Core
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

newtype JunkField = JunkField Text deriving (Eq, Ord, Show)

parseRawNote :: RawNote -> ([JunkField], Fields, NoteContent)
parseRawNote (RawNote rawText) =
  fromMaybe (error "Parsing of raw note failed. This should not be possible") $
    flip P.parseMaybe rawText $ do
      whitespaceP
      (j, f) <- frontmatter
      c <- content
      pure (j, f, c)
  where
    frontmatter :: Parser ([JunkField], Fields)
    frontmatter = (<|> pure ([], Fields M.empty)) $ P.try $ do
      delimiterLine
      fieldLines <- P.many $ do
        line <- Right <$> fieldP <|> Left <$> junkFieldP
        pure line
      let (junk, fields) = partitionEithers fieldLines
      delimiterLine
      pure (junk, Fields $ M.fromList fields)
    delimiterLine :: Parser ()
    delimiterLine = lexeme $ do
      line <- P.takeWhileP (Just "Delimiter line") (== '-')
      guard $ T.length line >= 3
      lineWhitespaceP
      void P.newline
      whitespaceP

    junkFieldP :: Parser JunkField
    junkFieldP = do
      P.notFollowedBy delimiterLine
      txt <- P.takeWhile1P (Just "Junk field") (\c -> c /= ';' && c /= '\n')
      fieldSeparatorP
      pure $ JunkField txt

    fieldP :: Parser (FieldName, Value)
    fieldP = lexemeLine $ P.try $ do
      fn <- fieldNameP
      value <-
        P.choice
          [ do
              _ <- lexemeLine $ P.char ':'
              StringValue <$> wordP <|> IntValue <$> intP,
            pure
              NoValue
          ]
      fieldSeparatorP
      pure (fn, value)

    fieldSeparatorP :: Parser ()
    fieldSeparatorP = lexeme $ void P.newline <|> void (P.char ';')

    content :: Parser NoteContent
    content = NoteContent <$> P.takeRest

makeFrontmatter :: Fields -> B.Builder
makeFrontmatter (coerce -> fields) =
  let fieldText = foldMap (uncurry makeField) $ M.toList fields
      frontmatter = if M.size fields == 0 then "" else "---\n" <> fieldText <> "---\n\n"
   in frontmatter
  where
    makeField field NoValue = B.fromText (field ^. #name) <> "\n"
    makeField field (StringValue txt) = B.fromText (field ^. #name) <> ": " <> B.fromText txt <> "\n"
    makeField field (IntValue n) = B.fromText (field ^. #name) <> ": " <> B.fromString (show n) <> "\n"

makeRawNote :: Fields -> NoteContent -> RawNote
makeRawNote fields noteContent =
  RawNote $
    TL.toStrict $
      B.toLazyText $
        makeFrontmatter fields <> B.fromText (noteContent ^. coerced)

noteToRaw :: Note -> RawNote
noteToRaw note =
  makeRawNote
    (note ^. #info % #fields)
    (note ^. #content)

noteFromRaw :: NoteInfo -> RawNote -> ([JunkField], Note)
noteFromRaw noteInfo rawNote =
  let (junk, fields, content) = parseRawNote rawNote
   in (junk, Note (noteInfo & #fields .~ fields) content)
