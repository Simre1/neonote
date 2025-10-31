module NeoNote.Run where

import Control.Monad (forM, forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Error.Dynamic
import NeoNote.Actions qualified as Action
import NeoNote.CLI
import NeoNote.CLI.Prompt
import NeoNote.Cache
import NeoNote.Configuration
import NeoNote.Data.Id
import NeoNote.Error
import NeoNote.Log
import NeoNote.Note.Highlight (Highlight, runHighlightWithCache)
import NeoNote.Note.Note
import NeoNote.Note.Syntax.NoteFilter
import NeoNote.Note.Syntax.RawNote
import NeoNote.Store.Database (Database, runDatabase)
import NeoNote.Store.Note
import NeoNote.Time
import Optics.Core
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath (takeExtension, (</>))

type AppEffects = [CLI, Highlight, NoteStore, MakeId, Database, Cache, GetTime, Error NeoNoteError, Log, GetConfiguration, IOE]

runNeoNote :: IO ()
runNeoNote = runIO $ withUnliftStrategy (ConcUnlift Persistent Unlimited) $ do
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
  Action.ViewNote amount plain frontmatter searchText -> runViewNoteAction amount plain frontmatter searchText
  Action.ListNotes attributesToShow showAmount orderBy searchText ->
    runListNotesAction searchText attributesToShow showAmount orderBy
  Action.AddNotes paths -> runAddNotesAction paths
  Action.ExportNotes path searchText -> runExportNotesAction path searchText

runCreateNoteAction :: (IOE :> es, CLI :> es, Log :> es, NoteStore :> es) => Text -> Bool -> Eff es ()
runCreateNoteAction initialText1 skipEditor = do
  createNote $ \noteInfo kont -> do
    initialText2 <- getStandardInput
    let initialText = initialText1 <> initialText2
    if skipEditor
      then kont EditorClosed (RawNote initialText)
      else editor (pure $ EditHandle {identifier = noteInfo, edit = kont, oldContent = RawNote initialText})

runEditNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, GetTime :> es) => Int -> Text -> Eff es ()
runEditNoteAction amount searchTerm = do
  noteFilter <- makeNoteFilter searchTerm
  selectedNotes <- findNotes noteFilter (Selection {amount, order = Descending AttributeModified})
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    (a : as) -> do
      let noteIds = a :| as
      notes <- traverse readNote noteIds

      editor $ flip fmap notes $ \note ->
        EditHandle
          { identifier = note ^. #info,
            oldContent = noteToRaw note,
            edit = \case
              EditorClosed -> writeNote note
              EditorOpen -> skipLogging . writeNote note
          }

runPickNoteAction :: (IOE :> es, CLI :> es, NoteStore :> es, GetTime :> es, Log :> es) => Text -> Eff es ()
runPickNoteAction initialSearchTerm = do
  selectedNotesRef <- liftIO $ newIORef []
  pick initialSearchTerm $
    PickerCallbacks
      { handlePickedAction = \case
          (PickedEdit noteInfo) -> do
            note <- readNote (noteInfo ^. #id)
            editor $
              pure $
                EditHandle
                  { identifier = note ^. #info,
                    oldContent = noteToRaw note,
                    edit = \case
                      EditorClosed -> writeNote note
                      EditorOpen -> skipLogging . writeNote note
                  }
            pure False
          (PickedDelete noteInfo) -> do
            note <- readNote (noteInfo ^. #id)
            answer <- prompt (AreYouSureDeletion (note :| []))
            when answer $ do
              deleteNote (noteInfo ^. #id)
            pure True
          (PickedView noteInfo) -> do
            note <- readNote (noteInfo ^. #id)
            displayNote False False note
            pure False
          PickedNothing -> pure False,
        findNotes = \searchTerm -> do
          time <- getCurrentTime
          case parseNoteFilter time searchTerm of
            Right newNoteFilter -> do
              noteIds <- findNotes newNoteFilter (Selection {amount = 10, order = Descending AttributeModified})
              noteInfos <- traverse readNoteInfo noteIds
              liftIO $ writeIORef selectedNotesRef noteInfos
            Left _ -> pure ()
          liftIO $ readIORef selectedNotesRef,
        getNoteContent = fmap (view #content) . readNote
      }

runDeleteNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, GetTime :> es) => Int -> Text -> Eff es ()
runDeleteNoteAction amount searchTerm = do
  noteFilter <- makeNoteFilter searchTerm
  selectedNotes <- findNotes noteFilter (Selection {amount, order = Descending AttributeModified})
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

runViewNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, GetTime :> es) => Int -> Bool -> Bool -> Text -> Eff es ()
runViewNoteAction amount plain frontmatter searchTerm = do
  noteFilter <- makeNoteFilter searchTerm
  selectedNotes <- take amount <$> findNotes noteFilter Selection {amount, order = Descending AttributeModified}
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    noteIds -> do
      forM_ noteIds $ \noteId -> do
        note <- readNote noteId
        displayNote plain frontmatter note

runListNotesAction :: (NoteStore :> es, CLI :> es, Error NeoNoteError :> es, GetTime :> es) => Text -> [NoteAttribute] -> Int -> OrderBy NoteAttribute -> Eff es ()
runListNotesAction search noteAttributes amount order = do
  let noteAttributes' =
        if null noteAttributes
          then [AttributeId]
          else noteAttributes
  noteFilter <- makeNoteFilter search
  noteIds <- findNotes noteFilter Selection {amount, order}
  noteInfos <- traverse readNoteInfo noteIds
  displayNotes noteAttributes' noteInfos

runAddNotesAction :: (IOE :> es, NoteStore :> es) => [FilePath] -> Eff es ()
runAddNotesAction paths = do
  noteData <- forM paths $ \filePath -> do
    let extension = pack $ drop 1 $ takeExtension filePath
    rawContent <- liftIO $ T.readFile filePath
    pure (extension, RawNote rawContent)
  bulkCreateNotes noteData

runExportNotesAction :: (IOE :> es, MakeId :> es, GetTime :> es, NoteStore :> es, Error NeoNoteError :> es) => Maybe FilePath -> Text -> Eff es ()
runExportNotesAction maybePath text = do
  noteFilter <- makeNoteFilter text
  noteIds <- findNotes noteFilter Selection {amount = maxBound, order = Descending AttributeModified}
  notes <- readNotes noteIds

  dirPath <- createDir maybePath
  liftIO $ forM_ notes $ \note -> do
    let filepath = dirPath </> (T.unpack $ noteFileName $ note ^. #info)
    let rawNote = noteToRaw note
    T.writeFile filepath (rawNote ^. coerced)

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
