module NeoNote.Run where

import Control.Monad (forM, forM_, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Error.Dynamic
import NeoNote.Actions qualified as Action
import NeoNote.CLI
import NeoNote.CLI.Picker (PickedAction (..))
import NeoNote.CLI.Prompt
import NeoNote.Cache
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Error
import NeoNote.Log
import NeoNote.Note.Highlight (Highlight, runHighlightWithCache)
import NeoNote.Note.Note
import NeoNote.Note.Parse (parseNoteFilter)
import NeoNote.Store.Database (Database, runDatabase)
import NeoNote.Store.Note
import NeoNote.Time
import Optics.Core
import System.Directory (createDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath (takeExtension, (</>))

type AppEffects = [CLI, Highlight, NoteStore, MakeId, Database, Cache, GetTime, Error NeoNoteError, Log, GetConfiguration, IOE]

runNeoNote :: IO ()
runNeoNote = do
  runIO $ do
    action <- getActionFromArguments
    handleAction action

runIO :: Eff AppEffects () -> IO ()
runIO =
  runEff
    . injectConfiguration mempty
    . runLog
    . runNeoNoteError
    . runGetTime
    . runCache
    . runDatabase
    . runMakeId
    . runNoteStore
    . runHighlightWithCache
    . runCLI

handleAction :: Action.Action -> Eff AppEffects ()
handleAction action = case action of
  Action.CreateNote skipEditor initialText -> runCreateNoteAction initialText skipEditor
  Action.EditNote amount searchText -> runEditNoteAction amount searchText
  Action.PickNote searchText -> runPickNoteAction searchText
  Action.DeleteNote amount searchText -> runDeleteNoteAction amount searchText
  Action.ViewNote amount plain searchText -> runViewNoteAction amount plain searchText
  Action.ListNotes attributesToShow showAmount orderBy searchText ->
    runListNotesAction searchText attributesToShow showAmount orderBy
  Action.AddNotes paths -> runAddNotesAction paths
  Action.ExportNotes path searchText -> runExportNotesAction path searchText

runCreateNoteAction :: (CLI :> es, Log :> es, NoteStore :> es) => Text -> Bool -> Eff es ()
runCreateNoteAction initialText skipEditor = do
  createNote $ \noteInfo -> do
    let initialNoteContent = NoteContent initialText
    (noteContent :| _) <-
      if skipEditor
        then pure $ pure initialNoteContent
        else editor $ pure $ Note noteInfo initialNoteContent
    pure noteContent

runEditNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, GetTime :> es) => Int -> Text -> Eff es ()
runEditNoteAction amount searchTerm = do
  noteFilter <- makeNoteFilter searchTerm
  selectedNotes <- take amount <$> findNotes noteFilter
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    (a : as) -> do
      let noteIds = a :| as
      notes <- traverse readNote noteIds
      newNoteContents <- editor notes

      forM_ (NE.zip (notes ^. mapping #info) newNoteContents) $ uncurry writeNote

runPickNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, GetTime :> es) => Text -> Eff es ()
runPickNoteAction searchTerm = do
  noteFilter <- makeNoteFilter searchTerm
  pick noteFilter searchTerm $ \case
    (Just (PickedEdit noteInfo)) -> do
      note <- readNote (noteInfo ^. #id)
      (newNoteContent :| _) <- editor $ pure note
      writeNote noteInfo newNoteContent
      pure False
    (Just (PickedDelete noteInfo)) -> do
      note <- readNote (noteInfo ^. #id)
      answer <- prompt (AreYouSureDeletion (note :| []))
      when answer $ do
        deleteNote (noteInfo ^. #id)
      pure True
    (Just (PickedView noteInfo)) -> do
      note <- readNote (noteInfo ^. #id)
      displayNote False note
      pure False
    _ -> False <$ logMessage NoMatchingNote

runDeleteNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, GetTime :> es) => Int -> Text -> Eff es ()
runDeleteNoteAction amount searchTerm = do
  noteFilter <- makeNoteFilter searchTerm
  selectedNotes <- take amount <$> findNotes noteFilter
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    noteIds -> do
      notes <- traverse readNote noteIds
      answer <-
        prompt
          ( AreYouSureDeletion
              (NE.fromList notes)
          )
      when answer $ do
        mapM_ deleteNote noteIds

runViewNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, GetTime :> es) => Int -> Bool -> Text -> Eff es ()
runViewNoteAction amount plain searchTerm = do
  noteFilter <- makeNoteFilter searchTerm
  selectedNotes <- take amount <$> findNotes noteFilter
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    noteIds -> do
      forM_ noteIds $ \noteId -> do
        note <- readNote noteId
        displayNote plain note

runListNotesAction :: (NoteStore :> es, CLI :> es, Error NeoNoteError :> es, GetTime :> es) => Text -> [NoteAttribute] -> Int -> OrderBy NoteAttribute -> Eff es ()
runListNotesAction search noteAttributes showAmount orderBy = do
  let noteAttributes' =
        if null noteAttributes
          then [AttributeId]
          else noteAttributes
  noteFilter <- makeNoteFilter search
  noteIds <- findNotes noteFilter
  noteInfos <- traverse readNoteInfo noteIds
  let orderedNoteInfos = orderNotes orderBy showAmount noteInfos
  displayNotes noteAttributes' orderedNoteInfos

runAddNotesAction :: (IOE :> es, NoteStore :> es) => [FilePath] -> Eff es ()
runAddNotesAction paths = do
  noteData <- forM paths $ \filePath -> do
    let extension = pack $ takeExtension filePath
    content <- liftIO $ T.readFile filePath
    pure (extension, NoteContent content)
  bulkCreateNotes noteData

runExportNotesAction :: (IOE :> es, MakeId :> es, GetTime :> es, NoteStore :> es, Error NeoNoteError :> es) => Maybe FilePath -> Text -> Eff es ()
runExportNotesAction maybePath text = do
  noteFilter <- makeNoteFilter text
  noteIds <- findNotes noteFilter
  notes <- readNotes noteIds

  dirPath <- createDir maybePath
  liftIO $ forM_ notes $ \note -> do
    let filepath = dirPath </> (T.unpack $ noteFileName $ note ^. #info)
    T.writeFile filepath (note ^. #content % coerced)

makeNoteFilter :: (GetTime :> es, Error NeoNoteError :> es) => Text -> Eff es NoteFilter
makeNoteFilter searchText = do
  time <- getCurrentTime
  case parseNoteFilter time searchText of
    Left err -> throwError (CannotParseFilter searchText err)
    Right noteFilter -> pure noteFilter

createDir :: (GetTime :> es, MakeId :> es, IOE :> es) => Maybe FilePath -> Eff es FilePath
createDir maybePath = do
  dirPath <- case maybePath of
    Just path -> pure path
    Nothing -> do
      cwd <- liftIO getCurrentDirectory
      time <- getCurrentTime
      pathId <- makeId
      let prefix = "neonote-export" <> "-" <> idToText pathId
      let timestamp = formatTimestamp prefix time
      pure $ cwd </> T.unpack timestamp

  liftIO $ createDirectoryIfMissing True dirPath
  pure dirPath
