module NeoNote.CLI.DisplayNotes where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.List (intersperse, sortBy, transpose)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.IO qualified as TL
import Effectful
import Effectful.Error.Dynamic (Error)
import NeoNote.Data.Id (idToText)
import NeoNote.Error (NeoNoteError)
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Store.Note (NoteStore, findNotes, readNote, readNoteInfo)
import NeoNote.Time
import Optics.Core

displayNotesInTerminal :: (IOE :> es) => [NoteAttribute] -> Ordered NoteInfo -> Eff es ()
displayNotesInTerminal attributes orderedNoteInfos = do
  let header = makeTitle <$> attributes
      rows = header : [attributeToCell noteInfo <$> attributes | noteInfo <- orderedNoteInfos ^. #list]
      tableText = renderTable rows
  liftIO $ TL.putStrLn tableText
  where
    renderTable :: [[Text]] -> TL.Text
    renderTable rows =
      let columns = transpose rows
          colWidths = map (maximum . map T.length) columns
          paddedColumns = zipWith (fmap . padSpaces) colWidths columns
          paddedRows = transpose paddedColumns
          table = mconcat $ intersperse (B.fromText "\n") $ fmap (mconcat . intersperse (B.fromText " ")) paddedRows
       in B.toLazyText table
    attributeToCell :: NoteInfo -> NoteAttribute -> Text
    attributeToCell noteInfo AttributeId = idToText $ coerce $ noteInfo ^. #id
    attributeToCell noteInfo AttributeCreated = formatDate $ noteInfo ^. #created
    attributeToCell noteInfo AttributeModified = formatDate $ noteInfo ^. #modified
    attributeToCell noteInfo AttributeExtension = noteInfo ^. #extension
    attributeToCell noteInfo AttributeTags = concatTags $ noteInfo ^. #tags
    makeTitle AttributeId = "id"
    makeTitle AttributeCreated = "created"
    makeTitle AttributeModified = "modified"
    makeTitle AttributeExtension = "extension"
    makeTitle AttributeTags = "tags"
    padSpaces maxWidth t =
      let n = max 0 $ maxWidth - T.length t
       in B.fromText t <> B.fromString (const ' ' <$> [1 .. n])
