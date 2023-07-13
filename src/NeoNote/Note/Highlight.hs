module NeoNote.Note.Highlight where

import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import NeoNote.Cache
import NeoNote.Note.Note
import Optics.Core
import Skylighting

data Highlight :: Effect where
  Highlight :: NoteInfo -> NoteContent -> Highlight m Text

makeEffect ''Highlight

runHighlightWithCache :: (Cache :> es) => Eff (Highlight : es) a -> Eff es a
runHighlightWithCache = interpret $ \_ (Highlight noteInfo noteContent) -> do
  cache "highlight" noteInfo (pure $ highlightNoteContent noteInfo noteContent)

runHighlight :: Eff (Highlight : es) a -> Eff es a
runHighlight = interpret $ \_ (Highlight noteInfo noteContent) ->
  pure $
    highlightNoteContent noteInfo noteContent

highlightNoteContent :: NoteInfo -> NoteContent -> Text
highlightNoteContent noteInfo noteContent = fromMaybe (coerce noteContent) $ do
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
