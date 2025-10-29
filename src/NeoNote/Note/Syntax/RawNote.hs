module NeoNote.Note.Syntax.RawNote where

import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Coerce
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import NeoNote.Note.Note
import NeoNote.Note.Syntax.Common
import Optics.Core
import Text.Megaparsec qualified as P

parseRawNote :: RawNote -> (M.Map Text Value, NoteContent)
parseRawNote (RawNote rawText) =
  fromMaybe (error "Parsing of raw note failed. This should not be possible") $
    flip P.parseMaybe rawText $ do
      whitespace
      f <- frontmatter
      c <- content
      pure (f, c)
  where
    frontmatter :: Parser (M.Map Text Value)
    frontmatter = (<|> pure M.empty) $ P.try $ do
      delimiterLine
      fields <- P.many field
      delimiterLine
      pure (M.fromList fields)
    delimiterLine :: Parser ()
    delimiterLine = lexeme $ do
      line <- P.takeWhileP (Just "Delimiter line") (== '-')
      guard $ T.length line >= 3

    field :: Parser (Text, Value)
    field = lexeme $ do
      fn <- fieldName
      value <-
        P.choice
          [ pure NoValue
          ]
      pure (fn, value)

    content :: Parser NoteContent
    content = NoteContent <$> P.takeRest

assembleRawNote :: M.Map Text Value -> NoteContent -> RawNote
assembleRawNote fields noteContent =
  RawNote $
    TL.toStrict $
      B.toLazyText $
        let fieldText = foldMap (uncurry assembleField) $ M.toList fields
            frontmatter = if M.size fields == 0 then "" else "---\n" <> fieldText <> "---\n\n"
         in frontmatter <> B.fromText (noteContent ^. coerced)
  where
    assembleField field NoValue = B.fromText field <> "\n"

noteToRaw :: Note -> RawNote
noteToRaw note =
  assembleRawNote
    fields
    (note ^. #content)
  where
    fields =
      let tags = note ^. #info % #tags
       in M.fromList $ coerce $ fmap (,NoValue) $ S.toList tags

noteFromRaw :: NoteInfo -> RawNote -> Note
noteFromRaw noteInfo rawNote =
  let (fields, content) = parseRawNote rawNote
   in Note (noteInfo & #tags .~ (S.fromList $ Tag <$> M.keys fields)) content
