module NeoNote.Note.Syntax.Common where

import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Char (isAlphaNum)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import NeoNote.Note.Note
import NeoNote.Time
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Parser = P.Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = P.lexeme $ whitespaceP

whitespaceP :: Parser ()
whitespaceP = void $ P.takeWhileP (Just "Whitespace") (<= ' ')

fieldNameP :: Parser FieldName
fieldNameP = P.try $ do
  text <- P.takeWhile1P (Just "Field") (`S.member` characters)
  guard $ T.head text /= '-'
  guard $ T.last text /= '-'
  guard $ not $ text `S.member` S.fromList ["created", "c", "modified", "c"]
  pure $ FieldName text
  where
    characters :: S.Set Char
    characters = S.fromList $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['-']

quotedStringP :: Parser Text
quotedStringP = do
  _ <- P.char '"'
  innerText <- P.takeWhileP (Just "Quoted string") (/= '"')
  _ <- P.char '"'
  pure $ innerText

wordP :: Parser Text
wordP = P.takeWhile1P (Just "Single word") isAlphaNum

intP :: Parser Int
intP = P.decimal

dateP :: Time -> Parser DateLiteral
dateP _ = do
  DateLiteralTime
    <$> dayP
      <|> DateLiteralTime
    <$> timeOfDayP
      <|> createdP
      <|> modifiedP
  where
    dayP :: Parser IncompleteTime
    dayP = P.try $ do
      dayString <- P.takeP (Just "Day") 10
      maybe (fail "Day could not be parsed") pure (dayFromString dayString)

    timeOfDayP :: Parser IncompleteTime
    timeOfDayP = P.try $ do
      timeOfDayString <- P.takeP (Just "Day") 8
      maybe (fail "Time of day could not be parsed") pure (timeOfDayFromString timeOfDayString)

    createdP :: Parser DateLiteral
    createdP = DateLiteralCreated <$ (P.string "created" <|> P.string "c")

    modifiedP :: Parser DateLiteral
    modifiedP = DateLiteralModified <$ (P.string "modified" <|> P.string "m")

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        <|> return x
