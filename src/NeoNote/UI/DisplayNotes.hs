module NeoNote.UI.DisplayNotes where

import Data.Coerce (coerce)
import Data.Default
import Data.List (sortBy)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Dynamic (Error)
import NeoNote.Data.Id (idToText)
import NeoNote.Error (NeoNoteError)
import NeoNote.Note.Note
import NeoNote.Search
import NeoNote.Time
import Optics.Core
import Text.Layout.Table
import NeoNote.Log
import NeoNote.Store.Note (NoteStore)

displayNotesInTerminal :: (IOE :> es, Error NeoNoteError :> es, Log :> es, NoteStore :> es) => NoteFilter -> Text -> OrderBy NoteAttribute -> Int -> [NoteAttribute] -> Eff es ()
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
    makeNoteRows getNoteContent noteInfo = do
      let attributeCells = attributeToCell noteInfo <$> noteAttributes
      noteContentCell <- getNoteContent $ noteInfo ^. #id
      pure $ rowG $ attributeCells ++ [unpack $ T.replace "\n" "  " $ T.take 60 $ coerce noteContentCell]
    makeTitle AttributeId = "id"
    makeTitle AttributeCreated = "created"
    makeTitle AttributeModified = "modified"
    makeTitle AttributeExtension = "extension"
    makeTitle AttributeTags = "tags"

    attributeToCell :: NoteInfo -> NoteAttribute -> String
    attributeToCell noteInfo AttributeId = unpack $ idToText $ coerce $ noteInfo ^. #id
    attributeToCell noteInfo AttributeCreated = unpack $ timeToString $ noteInfo ^. #created
    attributeToCell noteInfo AttributeModified = unpack $ timeToString $ noteInfo ^. #modified
    attributeToCell noteInfo AttributeExtension = unpack $ noteInfo ^. #extension
    attributeToCell noteInfo AttributeTags = unpack $ concatTags $ noteInfo ^. #tags

    (applyOrder, orderNoteAttribute) = case orderBy of
      Ascending a -> (reverse, a)
      Descending a -> (id, a)
