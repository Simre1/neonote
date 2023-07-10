module NeoNote.UI.DisplayNotes where

import Data.Coerce (coerce)
import Data.Default
import Data.List (intersperse, sortBy)
import Data.Set qualified as S
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Dynamic (Error)
import NeoNote.Actions
import NeoNote.Data.Id (idToText)
import NeoNote.Error (NeoNoteError)
import NeoNote.Note.Note
import NeoNote.Search
import NeoNote.Store.Database
import NeoNote.Store.Files
import NeoNote.Time
import Optics.Core
import Text.Layout.Table
import NeoNote.Log

displayNotesInTerminal :: (IOE :> es, Database :> es, Files :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> OrderBy NoteAttribute -> Int -> [NoteAttribute] -> Eff es ()
displayNotesInTerminal noteFilter search orderBy displayAmount noteAttributes' = do
  preparedSearch <- prepareSearch noteFilter

  notes <- (preparedSearch ^. #searchNotes) search
  let notesToDisplay =
        applyOrder $ take displayAmount $ sortBy (orderNote orderNoteAttribute) notes

  rows <- traverse (makeNoteRows (preparedSearch ^. #getNoteContent)) notesToDisplay
  let table =
        tableString
          ((makeColumn <$> noteAttributes) ++ [column (expandUntil 40) left def def])
          unicodeS
          (titlesH $ (makeTitle <$> noteAttributes) ++ ["content"])
          rows

  if null notesToDisplay
    then logMessage NoMatchingNote
    else liftIO $ putStrLn table
  where
    noteAttributes = case noteAttributes' of
      [] -> [AttributeModified, AttributeTags]
      _ -> noteAttributes'
    makeColumn AttributeTags = column (expandUntil 30) left def def
    makeColumn _ = def
    makeNoteRows getNoteContent (noteId, noteInfo) = do
      let attributeCells = attributeToCell (noteId, noteInfo) <$> noteAttributes
      noteContentCell <- getNoteContent noteId
      pure $ rowG $ attributeCells ++ [unpack $ T.replace "\n" "  " $ T.take 60 $ coerce noteContentCell]
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
    attributeToCell (_, noteInfo) AttributeTags = unpack $ concatTags $ noteInfo ^. #tags

    (applyOrder, orderNoteAttribute) = case orderBy of
      Ascending a -> (reverse, a)
      Descending a -> (id, a)
