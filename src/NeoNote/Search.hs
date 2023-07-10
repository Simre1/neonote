module NeoNote.Search where
import Effectful
import NeoNote.Store.Database
import NeoNote.Store.Files
import Effectful.Error.Dynamic
import NeoNote.Error
import NeoNote.Note.Note
import Data.Text (Text)
import Text.Fuzzy qualified as Fuzzy
import Data.Coerce (coerce)
import Control.Monad (zipWithM)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Optics.Core ((^.))
import Data.List (sortBy)

data PreparedSearch f = PreparedSearch {
  getNoteContent :: NoteId -> f NoteContent,
  searchNotes :: Text -> f [(NoteId, NoteInfo)]
  } deriving Generic

fuzzySearchNotes :: [(a, NoteContent)] -> Text -> [Fuzzy.Fuzzy (a, NoteContent) Text]
fuzzySearchNotes notes searchTerm =
    Fuzzy.filter
      searchTerm
      notes
      ""
      ""
      (coerce . snd)
      False

mapPreparedSearch :: (forall a. f a -> g a) -> PreparedSearch f -> PreparedSearch g
mapPreparedSearch f preparedSearch = PreparedSearch {
  getNoteContent = f . (preparedSearch ^. #getNoteContent),
  searchNotes = f . (preparedSearch ^. #searchNotes)
}

prepareSearch :: (Database :> es, Files :> es, Error NeoNoteError :> es) => NoteFilter ->  Eff es (PreparedSearch (Eff es2))
prepareSearch noteFilter = do
  
  noteIds <- findNotes noteFilter
  noteInfos <- traverse getNoteInfo noteIds
  noteContents <- zipWithM readNote noteIds noteInfos

  let notes = zip (zip noteIds noteInfos) (coerce <$> noteContents)
      noteMap = M.fromList $ zip noteIds noteContents
      getNoteContent noteId = pure $ noteMap M.! noteId
      searchNotes = pure . fmap (fst . Fuzzy.original) .  sortBy fuzzyNoteCompare  . fuzzySearchNotes notes
  
  pure $ PreparedSearch {
    getNoteContent = getNoteContent,
    searchNotes = searchNotes
  }
  where 
    fuzzyNoteCompare fuzzy1 fuzzy2 = case compare (Fuzzy.score fuzzy2) (Fuzzy.score fuzzy1) of
      EQ -> orderNote AttributeModified (fst $ Fuzzy.original fuzzy1) (fst $ Fuzzy.original fuzzy2)
      ordering -> ordering
