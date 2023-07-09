module NeoNote.UI.DisplayNotes where

import Data.Coerce (coerce)
import Data.Default
import Data.List (intersperse, sortBy)
import Data.Set qualified as S
import Data.Text (unpack)
import Effectful
import NeoNote.Actions
import NeoNote.Data.Id (idToText)
import NeoNote.Note.Note
import NeoNote.Time
import Optics.Core
import Text.Layout.Table

displayNotesInTerminal :: (IOE :> es) => OrderBy NoteAttribute -> Int -> [NoteAttribute] -> [(NoteId, NoteInfo)] -> Eff es ()
displayNotesInTerminal orderBy displayAmount noteAttributes' notes = do
  let notesToDisplay =
        applyOrder $ take displayAmount $ sortBy (orderNote orderNoteAttribute) notes

  liftIO $ putStrLn $ tableString (makeColumn <$> noteAttributes) unicodeS (titlesH $ makeTitle <$> noteAttributes) (makeNoteRows <$> notesToDisplay)
  where
    noteAttributes = case noteAttributes' of
      [] -> [AttributeId]
      _ -> noteAttributes'
    makeColumn AttributeTags = column (expandUntil 30) left def def
    makeColumn _ = def
    makeNoteRows (noteId, noteInfo) = rowG $ attributeToCell (noteId, noteInfo) <$> noteAttributes
    makeTitle AttributeId = "id"
    makeTitle AttributeCreated = "created"
    makeTitle AttributeModified = "modified"
    makeTitle AttributeExtension = "extension"
    makeTitle AttributeTags = "tags"

    attributeToCell :: (NoteId, NoteInfo) -> NoteAttribute -> String
    attributeToCell (noteId, _) AttributeId = unpack $ idToText $ coerce noteId
    attributeToCell (_, noteInfo) AttributeCreated = unpack $ timeToString $ noteInfo ^. #created
    attributeToCell (_, noteInfo) AttributeModified = unpack $ timeToString $ noteInfo ^. #modified
    attributeToCell (_, noteInfo) AttributeExtension = unpack $ noteInfo ^. #extension
    attributeToCell (_, noteInfo) AttributeTags =
      unpack $
        mconcat $
          intersperse "," $
            coerce
              <$> S.toList (noteInfo ^. #tags)

    (applyOrder, orderNoteAttribute) = case orderBy of
      Ascending a -> (reverse, a)
      Descending a -> (id, a)
