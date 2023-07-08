{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module NeoNote.Note.Parse where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Set qualified as S
import Data.Text
import Data.Text qualified as T
import Data.Void (Void)
import NeoNote.Note.Note
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Regex.TDFA

extractTags :: NoteContent -> S.Set Tag
extractTags (NoteContent noteContent) =
  let matches = noteContent =~ tagRegex
   in S.fromList $ Tag . T.tail <$> getAllTextMatches matches
  where
    -- TODO: Can only match ASCII (english) tags
    tagRegex :: Text
    tagRegex = "#(([a-z]|[A-Z])+(-([a-z]|[A-Z])+)*)"

parseNoteFilter :: Text -> Either Text NoteFilter
parseNoteFilter = first (pack . P.errorBundlePretty) . P.parse parser "note filter string"
  where
    parser :: P.Parsec Void Text NoteFilter
    parser = do
      let tagP = P.try $ do
            tagString <- P.takeWhile1P (Just "Tag") (`S.member` tagCharacters)
            guard $ T.head tagString /= '-'
            guard $ T.last tagString /= '-'
            pure $ HasTag $ Tag tagString
          notP = P.try $ P.char '~' >> Not <$> combinedP
          everynoteP = P.try $ EveryNote <$ P.string "*"
          bracketsP = P.try $ do
            _ <- P.char '('
            p <- combinedP
            _ <- P.char ')'
            pure p
          andOrP = chainl1 nonLeftRecursiveP (P.try $ And <$ P.char '&' <|> Or <$ P.char '|')
          nonLeftRecursiveP = P.try $ bracketsP <|> tagP <|> everynoteP <|> notP
          combinedP = P.try $ andOrP <|> nonLeftRecursiveP
      p <- combinedP
      P.eof
      pure p
    tagCharacters :: S.Set Char
    tagCharacters = S.fromList $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['-']

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
