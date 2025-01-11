module NeoNote.Run where

import Control.Monad (forM_, when)
import Data.List.NonEmpty as NE (NonEmpty (..), fromList, zip)
import Data.Text (Text)
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
import NeoNote.Search
import NeoNote.Store.Database (Database, runDatabase)
import NeoNote.Store.Files (Files, runFiles)
import NeoNote.Store.Note
import NeoNote.Time


foreign import ccall "hello" hello :: IO ()

type AppEffects = [CLI, NoteSearch, Highlight, NoteStore, MakeId, Database, Files, Cache, GetTime, Error NeoNoteError, Log, GetConfiguration, IOE]

runNeoNote :: IO ()
runNeoNote = do
  hello
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
    . runFiles
    . runDatabase
    . runMakeId
    . runNoteStore
    . runHighlightWithCache
    . runNoteSearch
    . runCLI

handleAction :: Action.Action -> Eff AppEffects ()
handleAction action = case action of
  Action.CreateNote skipEditor initialText -> runCreateNoteAction initialText skipEditor
  Action.EditNote noteFilter amount searchText -> runEditNoteAction noteFilter amount searchText
  Action.PickNote noteFilter searchText -> runPickNoteAction noteFilter searchText
  Action.DeleteNote noteFilter amount searchText -> runDeleteNoteAction noteFilter amount searchText
  Action.ViewNote noteFilter amount plain searchText -> runViewNoteAction noteFilter amount plain searchText
  Action.ListNotes noteFilter attributesToShow showAmount orderBy searchText ->
    runListNotesAction noteFilter searchText attributesToShow showAmount orderBy
  Action.ScanNotes -> runScanNotesAction

runCreateNoteAction :: (CLI :> es, Log :> es, NoteStore :> es) => Text -> Bool -> Eff es ()
runCreateNoteAction initialText skipEditor = do
  createNote $ \noteInfo -> do
    let initialNoteContent = NoteContent initialText
    (noteContent :| _) <-
      if skipEditor
        then pure $ pure initialNoteContent
        else editor $ pure (noteInfo, initialNoteContent)
    pure noteContent

runEditNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, NoteSearch :> es) => NoteFilter -> Int -> Text -> Eff es ()
runEditNoteAction noteFilter amount searchTerm = do
  selectedNotes <- take amount <$> searchNotes noteFilter searchTerm
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    (a : as) -> do
      let noteInfos = a :| as
      noteContents <- traverse readNote noteInfos
      newNoteContents <- editor $ NE.zip noteInfos noteContents

      forM_ (NE.zip noteInfos newNoteContents) $ uncurry writeNote

runPickNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es) => NoteFilter -> Text -> Eff es ()
runPickNoteAction noteFilter searchTerm = do
  pick noteFilter searchTerm $ \case
    (Just (PickedEdit noteInfo)) -> do
      noteContent <- readNote noteInfo
      (newNoteContent :| _) <- editor $ pure (noteInfo, noteContent)
      writeNote noteInfo newNoteContent
      pure False
    (Just (PickedDelete noteInfo)) -> do
      noteContent <- readNote noteInfo
      answer <- prompt (AreYouSureDeletion ((noteInfo, noteContent) :| []))
      when answer $ do
        deleteNote noteInfo
      pure True
    (Just (PickedView noteInfo)) -> do
      noteContent <- readNote noteInfo
      displayNote False noteInfo noteContent
      pure False
    _ -> False <$ logMessage NoMatchingNote

runDeleteNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, NoteSearch :> es) => NoteFilter -> Int -> Text -> Eff es ()
runDeleteNoteAction noteFilter amount searchTerm = do
  selectedNotes <- take amount <$> searchNotes noteFilter searchTerm
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    noteInfos -> do
      noteContents <- traverse readNote noteInfos
      answer <-
        prompt
          ( AreYouSureDeletion
              (NE.fromList $ Prelude.zip noteInfos noteContents)
          )
      when answer $ do
        mapM_ deleteNote noteInfos

runViewNoteAction :: (CLI :> es, NoteStore :> es, Error NeoNoteError :> es, Log :> es, NoteSearch :> es) => NoteFilter -> Int -> Bool -> Text -> Eff es ()
runViewNoteAction noteFilter amount plain searchTerm = do
  selectedNotes <- take amount <$> searchNotes noteFilter searchTerm
  case selectedNotes of
    [] -> logMessage NoMatchingNote
    noteInfos -> do
      forM_ noteInfos $ \noteInfo -> do
        noteContent <- readNote noteInfo
        displayNote plain noteInfo noteContent

runListNotesAction :: (CLI :> es, Error NeoNoteError :> es) => NoteFilter -> Text -> [NoteAttribute] -> Int -> OrderBy NoteAttribute -> Eff es ()
runListNotesAction noteFilter search noteAttributes showAmount orderBy = do
  displayNotes noteFilter search orderBy showAmount noteAttributes

runScanNotesAction :: (IOE :> es) => Eff es ()
runScanNotesAction = liftIO $ putStrLn "Sorry, not yet implemented"
