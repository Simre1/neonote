module NeoNote.CLI.DisplayNotes where

import Data.Coerce (coerce)
import Data.List (intersperse, transpose)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.IO qualified as TL
import Effectful
import NeoNote.Data.Id (idToText)
import NeoNote.Note.Note
import NeoNote.Time
import Optics.Core

displayNotesInTerminal :: (IOE :> es) => [NoteAttribute] -> [NoteInfo] -> Eff es ()
displayNotesInTerminal attributes orderedNoteInfos = do
  let _header = makeTitle <$> attributes
      rows = [attributeToCell noteInfo <$> attributes | noteInfo <- orderedNoteInfos]
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
    attributeToCell noteInfo AttributeProperties =
      let tagString = concatFields $ noteInfo ^. #fields
       in if tagString == "" then "-" else tagString
    makeTitle AttributeId = "id"
    makeTitle AttributeCreated = "created"
    makeTitle AttributeModified = "modified"
    makeTitle AttributeExtension = "extension"
    makeTitle AttributeProperties = "properties"
    padSpaces maxWidth t =
      let n = max 0 $ maxWidth - T.length t
       in B.fromText t <> B.fromString (const ' ' <$> [1 .. n])
