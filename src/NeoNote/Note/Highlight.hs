module NeoNote.Note.Highlight where

import Data.Coerce (coerce)
import Data.Text (Text)
import NeoNote.Note.Note
import Optics.Core
import Skylighting
import Data.Maybe (fromMaybe)

highlight :: NoteInfo -> NoteContent -> Text
highlight noteInfo noteContent = fromMaybe (coerce noteContent) $ do
  syntax <- lookupSyntax (noteInfo ^. #extension) syntaxMap
  either (const Nothing) Just $
    formatANSI formatOptions style <$> source syntax
  where
    formatOptions = defaultFormatOpts
    style = pygments
    source syntax = tokenize tokenizerConfig syntax $ coerce noteContent
    tokenizerConfig =
      TokenizerConfig
        { syntaxMap = syntaxMap,
          traceOutput = False
        }
    syntaxMap = defaultSyntaxMap
