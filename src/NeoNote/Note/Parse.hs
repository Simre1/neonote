module NeoNote.Note.Parse where

import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.Bifunctor (first)
import Data.Set qualified as S
import Data.Text
import Data.Text qualified as T
import Data.Void (Void)
import NeoNote.Note.Note
import NeoNote.Time
import Text.Megaparsec (choice)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Regex.TDFA

extractTags :: NoteContent -> S.Set Tag
extractTags (NoteContent noteContent) =
  let matches = noteContent =~ tagRegex
   in S.fromList $ Tag . T.tail <$> getAllTextMatches matches
  where
    -- TODO: Can only match ASCII (english) tags
    tagRegex :: Text
    tagRegex = "#(([a-z]|[A-Z])+(-([a-z]|[A-Z])+)*)"

type Parser = P.Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = P.lexeme $ whitespace

whitespace :: Parser ()
whitespace = void $ P.takeWhileP (Just "Whitespace") (<= ' ')

parseNoteFilter :: Time -> Text -> Either Text NoteFilter
parseNoteFilter currentTime = first (pack . P.errorBundlePretty) . P.parse parser "note filter string"
  where
    parser :: Parser NoteFilter
    parser = do
      let tagP = P.try $ do
            _ <- P.char '#'
            tagString <- lexeme $ P.takeWhile1P (Just "Tag") (`S.member` tagCharacters)
            guard $ T.head tagString /= '-'
            guard $ T.last tagString /= '-'
            guard $ not $ tagString `S.member` S.fromList ["created", "c", "modified", "c"]
            pure $ HasTag $ Tag tagString
          timeP = do
            d1 <- dateParser currentTime
            operation <- lexeme $ EqualDate <$ P.char '=' <|> AfterDate <$ P.char '>' <|> BeforeDate <$ P.char '<'
            d2 <- dateParser currentTime
            pure $ operation d1 d2
          containsP =
            choice
              [ lexeme $
                  do
                    _ <- P.char '\"'
                    f <- lexeme $ P.takeWhile1P (Just "Quoted Search fragment") (> ' ')
                    _ <- P.char '\"'
                    pure $ Contains f,
                fmap
                  Contains
                  $ lexeme
                  $ P.takeWhile1P (Just "Search fragment") (> ' ')
              ]
          notP = lexeme $ P.char '~' >> Not <$> combinedP
          everynoteP = lexeme $ EveryNote <$ P.string "*"
          bracketsP = lexeme $ do
            _ <- P.char '('
            p <- combinedP
            _ <- P.char ')'
            pure p
          andOrP = chainl1 nonLeftRecursiveP (lexeme $ And <$ P.char '&' <|> Or <$ P.char '|' <|> pure Together)
          nonLeftRecursiveP = bracketsP <|> tagP <|> everynoteP <|> notP <|> timeP <|> containsP
          combinedP = andOrP <|> nonLeftRecursiveP
      choice
        [ do
            p <- combinedP
            P.eof
            pure p,
          do
            whitespace
            P.eof
            pure EveryNote
        ]
    tagCharacters :: S.Set Char
    tagCharacters = S.fromList $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['-']

dateParser :: Time -> Parser DateLiteral
dateParser _ = do
  DateLiteral <$> dayP
    <|> DateLiteral <$> timeOfDayP
    <|> createdP
    <|> modifiedP
  where
    dayP :: Parser IncompleteTime
    dayP = P.try $ lexeme $ do
      dayString <- P.takeP (Just "Day") 10
      maybe (fail "Day could not be parsed") pure (dayFromString dayString)

    timeOfDayP :: Parser IncompleteTime
    timeOfDayP = P.try $ lexeme $ do
      timeOfDayString <- P.takeP (Just "Day") 8
      maybe (fail "Time of day could not be parsed") pure (timeOfDayFromString timeOfDayString)

    createdP :: Parser DateLiteral
    createdP = lexeme $ DateLiteralCreated <$ (P.string "created" <|> P.string "c")

    modifiedP :: Parser DateLiteral
    modifiedP = lexeme $ DateLiteralModified <$ (P.string "modified" <|> P.string "m")

chainl1 :: P.Parsec Void Text a -> P.Parsec Void Text (a -> a -> a) -> P.Parsec Void Text a
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
