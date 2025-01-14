module NeoNote.CLI.DisplayNotes where

import Data.Coerce (coerce)
import Data.List (sortBy)
import Data.Text (Text, unpack)
import Effectful
import Effectful.Error.Dynamic (Error)
import NeoNote.Data.Id (idToText)
import NeoNote.Error (NeoNoteError)
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Store.Note (NoteStore, findNotes, readNote, readNoteInfo)
import NeoNote.Time
import Optics.Core
import Text.Layout.Table

displayNotesInTerminal :: (IOE :> es, Error NeoNoteError :> es, Log :> es, NoteStore :> es) => NoteFilter -> OrderBy NoteAttribute -> Int -> [NoteAttribute] -> Eff es ()
displayNotesInTerminal noteFilter orderBy displayAmount noteAttributes' = do
  noteIds <- findNotes noteFilter
  noteInfos <- traverse readNoteInfo noteIds
  let notesToDisplay =
        applyOrder $ take displayAmount $ sortBy (orderNote orderNoteAttribute) noteInfos

  rows <- traverse makeNoteRows notesToDisplay
  let table =
        tableString $
          columnHeaderTableS
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
    makeNoteRows noteInfo = do
      let attributeCells = attributeToCell noteInfo <$> noteAttributes
      note <- readNote (noteInfo ^. #id)
      pure $ rowG $ attributeCells ++ [unpack $ noteContentPreview (note ^. #content)]
    makeTitle AttributeId = "id" :: Text
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
