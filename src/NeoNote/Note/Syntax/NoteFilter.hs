module NeoNote.Note.Syntax.NoteFilter where

import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlphaNum)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace
import NeoNote.Data.Id
import NeoNote.Note.Note
import NeoNote.Note.Syntax.Common
import NeoNote.Time
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

parseNoteFilter :: Time -> Text -> Either Text NoteFilter
parseNoteFilter currentTime = first (T.pack . P.errorBundlePretty) . P.parse parser "note filter string"
  where
    parser :: Parser NoteFilter
    parser = do
      let hasFieldP = lexeme $ do
            _ <- P.char '#'
            fn <- fieldNameP
            pure $ HasField fn
          literalP = lexeme $ P.try $ do
            P.choice
              [ do
                  _ <- P.char '#'
                  fn <- fieldNameP
                  pure $ FieldLiteral fn,
                DateLiteral <$> dateP currentTime,
                do
                  _ <- P.char '#'
                  _ <- P.string "id"
                  pure IdLiteral,
                StringLiteral <$> quotedStringP,
                IntLiteral <$> intP,
                StringLiteral <$> wordP
              ]
          opP =
            lexeme $
              P.choice
                [ Equal <$ P.string "=",
                  Equal <$ P.string "==",
                  Greater <$ P.string ">",
                  Lesser <$ P.string "<",
                  GreaterEqual <$ P.string ">=",
                  LessserEqual <$ P.string "<="
                ]
          checkP = P.try $ do
            lit1 <- literalP
            op <- opP
            lit2 <- literalP
            pure (Check op lit1 lit2)

          -- timeP = P.try $ do
          --   d1 <- dateP currentTime
          --   operation <- lexeme $ EqualDate <$ P.char '=' <|> AfterDate <$ P.char '>' <|> BeforeDate <$ P.char '<'
          --   d2 <- dateP currentTime
          --   pure $ operation d1 d2
          containsP =
            P.choice
              [ lexeme $
                  Contains <$> quotedStringP,
                -- do
                --   _ <- P.char '\"'
                --   f <- lexeme $ P.takeWhile1P (Just "Quoted Search fragment") (> ' ')
                --   _ <- P.char '\"'
                --   pure $ Contains f,
                fmap
                  Contains
                  $ lexeme
                  $ wordP
              ]
          notP = lexeme (P.char '~') >> Not <$> nonLeftRecursiveP
          everynoteP = lexeme $ EveryNote <$ P.string "*"
          bracketsP = do
            _ <- lexeme $ P.char '('
            p <- combinedP
            _ <- lexeme $ P.char ')'
            pure p
          andOrP = chainl1 nonLeftRecursiveP (lexeme $ And <$ P.char '&' <|> Or <$ P.char '|' <|> pure Together)
          nonLeftRecursiveP = bracketsP <|> everynoteP <|> notP <|> checkP <|> hasFieldP <|> containsP
          combinedP = andOrP <|> nonLeftRecursiveP
      P.choice
        [ do
            p <- combinedP
            P.eof
            pure p,
          do
            whitespaceP
            P.eof
            pure EveryNote
        ]
