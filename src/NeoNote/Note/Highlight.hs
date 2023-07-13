module NeoNote.Note.Highlight where

import Data.Coerce (coerce)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import NeoNote.Cache
import NeoNote.Note.Note

data Highlight :: Effect where
  Highlight :: NoteInfo -> NoteContent -> Highlight m Text

makeEffect ''Highlight

runHighlightWithCache :: (Cache :> es) => Eff (Highlight : es) a -> Eff es a
runHighlightWithCache = interpret $ \_ (Highlight noteInfo noteContent) -> do
  -- traceShowId <$> cache "highlight" noteInfo (pure $ highlightNoteContent noteInfo noteContent)
  pure $ coerce noteContent

runHighlight :: Eff (Highlight : es) a -> Eff es a
runHighlight = interpret $ \_ (Highlight noteInfo noteContent) ->
  pure $
    highlightNoteContent noteInfo noteContent

highlightNoteContent :: NoteInfo -> NoteContent -> Text
highlightNoteContent noteInfo noteContent = coerce noteContent
