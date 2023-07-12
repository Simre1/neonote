module NeoNote.Search where

import Data.Coerce (coerce)
import Data.IORef
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import NeoNote.Error
import NeoNote.Note.Note
import NeoNote.Store.Note
import Text.Fuzzy qualified as Fuzzy

data NoteSearch :: Effect where
  GetNoteContent :: NoteInfo -> NoteSearch m NoteContent
  SearchNotes :: NoteFilter -> Text -> NoteSearch m [NoteInfo]
  SearchNotesNoCache :: NoteFilter -> Text -> NoteSearch m [NoteInfo]

makeEffect ''NoteSearch

data NoteSearchHandle = NoteSearchHandle
  { getNoteContent :: NoteInfo -> IO NoteContent,
    searchNotes :: NoteFilter -> Text -> IO [NoteInfo],
    searchNotesNoCache :: NoteFilter -> Text -> IO [NoteInfo]
  }
  deriving (Generic)

runNoteSearch :: (NoteStore :> es, IOE :> es, Error NeoNoteError :> es) => Eff (NoteSearch : es) a -> Eff es a
runNoteSearch eff = do
  currentlyFilteredNotes <- liftIO $ newIORef (Not EveryNote, M.empty :: M.Map NoteInfo NoteContent)

  interpret
    ( \_ -> \case
        GetNoteContent noteInfo -> do
          noteMap <- liftIO $ snd <$> readIORef currentlyFilteredNotes
          pure $ fromMaybe (NoteContent "This note id is not one that's currently in the search.") $ M.lookup noteInfo noteMap
        SearchNotes noteFilter text -> do
          (cachedNoteFilter, cachedNotes) <- liftIO $ readIORef currentlyFilteredNotes

          notes <-
            if cachedNoteFilter == noteFilter
              then pure cachedNotes
              else do
                noteIds <- findNotes noteFilter
                noteInfos <- traverse getNoteInfo noteIds
                noteContents <- traverse readNote noteInfos
                let notesMap = M.fromList $ zip noteInfos noteContents
                liftIO $ writeIORef currentlyFilteredNotes (noteFilter, notesMap)
                pure notesMap

          pure . fmap (fst . Fuzzy.original) . sortBy fuzzyNoteCompare $ fuzzySearchNotes (M.toList notes) text
        SearchNotesNoCache noteFilter text -> do
          notes <- do
            noteIds <- findNotes noteFilter
            noteInfos <- traverse getNoteInfo noteIds
            noteContents <- traverse readNote noteInfos
            let notesMap = M.fromList $ zip noteInfos noteContents
            liftIO $ writeIORef currentlyFilteredNotes (noteFilter, notesMap)
            pure notesMap

          pure . fmap (fst . Fuzzy.original) . sortBy fuzzyNoteCompare $ fuzzySearchNotes (M.toList notes) text
    )
    eff
  where
    fuzzyNoteCompare fuzzy1 fuzzy2 = case compare (Fuzzy.score fuzzy2) (Fuzzy.score fuzzy1) of
      EQ -> orderNote AttributeModified (fst $ Fuzzy.original fuzzy1) (fst $ Fuzzy.original fuzzy2)
      ordering -> ordering

withNoteSearchHandle :: (NoteSearch :> es, IOE :> es) => (NoteSearchHandle -> IO a) -> Eff es a
withNoteSearchHandle f = do
  withRunInIO $ \unlift ->
    f $
      NoteSearchHandle
        { getNoteContent = unlift . getNoteContent,
          searchNotes = \noteFilter search -> unlift $ searchNotes noteFilter search,
          searchNotesNoCache = \noteFilter search -> unlift $ searchNotesNoCache noteFilter search
        }

fuzzySearchNotes :: [(a, NoteContent)] -> Text -> [Fuzzy.Fuzzy (a, NoteContent) Text]
fuzzySearchNotes notes searchTerm =
  Fuzzy.filter
    searchTerm
    notes
    ""
    ""
    (coerce . snd)
    False
