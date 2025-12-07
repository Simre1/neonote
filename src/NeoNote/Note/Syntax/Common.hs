module NeoNote.Note.Syntax.Common where

import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Trans.State
import Data.Char (isAlphaNum, isNumber)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Debug.Trace
import NeoNote.Note.Note
import NeoNote.Time
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Parser = P.Parsec Void Text

type ParserS s = P.ParsecT Void Text (State s)

lexeme :: (P.MonadParsec e Text p) => p a -> p a
lexeme = P.lexeme whitespaceP

lexemeLine :: (P.MonadParsec e Text p) => p a -> p a
lexemeLine = P.lexeme lineWhitespaceP

lineWhitespaceP :: (P.MonadParsec e Text p) => p ()
lineWhitespaceP =
  void $
    P.takeWhileP
      (Just "Line whitespace")
      (\c -> c <= ' ' && c /= '\n')

whitespaceP :: (P.MonadParsec e Text p) => p ()
whitespaceP = void $ P.takeWhileP (Just "Whitespace") (<= ' ')

fieldNameP :: (P.MonadParsec e Text p) => p PropertyName
fieldNameP = P.try $ do
  text <- P.takeWhile1P (Just "Field") isSafeChar
  guard $ S.notMember (T.head text) specialCharacters
  guard $ S.notMember (T.head text) specialCharacters
  guard $ not $ text `S.member` S.fromList ["created", "c", "modified", "c", "id"]
  pure $ PropertyName text

valueP :: (P.MonadParsec e Text p) => p Value
valueP = StringValue <$> wordP <|> IntValue <$> intP

makeFieldName :: Text -> Maybe PropertyName
makeFieldName = P.parseMaybe @() fieldNameP

isSafeChar :: Char -> Bool
isSafeChar c = isAlphaNum c || S.member c specialCharacters

specialCharacters :: S.Set Char
specialCharacters = S.fromList $ "-+_."

quotedStringP :: (P.MonadParsec e Text p) => p Text
quotedStringP = do
  _ <- P.char '"'
  innerText <- P.takeWhileP (Just "Quoted string") (/= '"')
  _ <- P.char '"'
  pure $ innerText

wordP :: (P.MonadParsec e Text p) => p Text
wordP = P.takeWhile1P (Just "Single word") isSafeChar

intP :: (P.MonadParsec e Text p) => p Int
intP = P.decimal

timeP :: (MonadFail p, P.MonadParsec e Text p) => Time -> p IncompleteTime
timeP _ = dayP <|> timeOfDayP
  where
    dayP :: (MonadFail p, P.MonadParsec e Text p) => p IncompleteTime
    dayP = P.try $ do
      -- Lookahead to prevent megaparsec from treating junk like "~&a"
      -- as an incomplete day string and producing bad error messages
      _ <- P.lookAhead $ P.satisfy $ isNumber
      dayString <- P.takeP (Just "Day") 10
      maybe (fail "Day could not be parsed") pure (dayFromString dayString)

    timeOfDayP :: (MonadFail p, P.MonadParsec e Text p) => p IncompleteTime
    timeOfDayP = P.try $ do
      _ <- P.lookAhead $ P.satisfy $ isNumber
      timeOfDayString <- P.takeP (Just "Day") 8
      maybe (fail "Time of day could not be parsed") pure (timeOfDayFromString timeOfDayString)

chainl1 :: (Alternative p, Monad p) => p a -> p (a -> a -> a) -> p a
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

many1 :: (Alternative p) => p a -> p [a]
many1 p = liftA2 (:) p (many p)

surroundedBy :: (P.MonadParsec e Text p) => p s -> p a -> p a
surroundedBy s p = do
  _ <- s
  a <- p
  _ <- s
  pure a

stringLiteralP :: (P.MonadParsec e Text p) => p Text
stringLiteralP = do
  _ <- P.char '"'
  txt <- P.takeWhile1P (Just "String literal") (/= '\"')
  _ <- P.char '"'
  pure txt

numberP :: (P.MonadParsec e Text p) => p Int
numberP = P.signed (pure ()) P.decimal

message :: (P.MonadParsec e Text p) => String -> p ()
message str = do
  !() <- traceShow str (pure ())
  pure ()
