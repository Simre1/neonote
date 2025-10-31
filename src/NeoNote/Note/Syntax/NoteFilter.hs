module NeoNote.Note.Syntax.NoteFilter where

import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlphaNum)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import Effectful (runEff)
import NeoNote.Data.Id
import NeoNote.Note.Note
import NeoNote.Note.Syntax.Common
import NeoNote.Time
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

parseNoteFilterIO :: Text -> IO NoteFilter
parseNoteFilterIO txt = runEff $ runGetTime $ do
  time <- getCurrentTime
  let result = parseNoteFilter time txt
  either (\err -> liftIO $ T.putStrLn err >> fail "Parse failed") pure result

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
                  P.choice
                    [ IdLiteral <$ P.string "id",
                      DateLiteral DateLiteralCreated <$ (P.string "created" <|> P.string "c"),
                      DateLiteral DateLiteralModified <$ (P.string "modified" <|> P.string "m"),
                      FieldLiteral <$> fieldNameP
                    ],
                DateLiteral <$> DateLiteralTime <$> timeP currentTime,
                StringLiteral <$> quotedStringP,
                IntLiteral <$> intP,
                StringLiteral <$> wordP
              ]
          opP =
            lexeme $
              P.choice
                [ Equal <$ P.string "=",
                  Equal <$ P.string "==",
                  Similar <$ P.string "~",
                  Greater <$ P.string ">",
                  Lesser <$ P.string "<",
                  GreaterEqual <$ P.string ">=",
                  LesserEqual <$ P.string "<="
                ]
          checkP = P.try $ do
            lit1 <- literalP
            op <- opP
            lit2 <- literalP
            pure (Check op lit1 lit2)
          containsP =
            P.choice
              [ lexeme $
                  Contains <$> quotedStringP,
                fmap
                  Contains
                  $ lexeme
                  $ wordP
              ]
          notP = lexeme (P.char '!') >> Not <$> nonLeftRecursiveP
          everynoteP = lexeme $ EveryNote <$ P.string "*"
          bracketsP = do
            _ <- lexeme $ P.char '('
            p <- combinedP
            _ <- lexeme $ P.char ')'
            pure p
          andOrP =
            chainl1 nonLeftRecursiveP $
              lexeme $
                P.choice
                  [ And <$ (void (P.string "&&") <|> void (P.char '&')),
                    Or <$ (void (P.string "||") <|> void (P.char '|')),
                    pure Together
                  ]
          nonLeftRecursiveP = do
            bracketsP <|> everynoteP <|> notP <|> checkP <|> hasFieldP <|> containsP
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
