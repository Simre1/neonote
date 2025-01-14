module NeoNote.Run where

import Control.Monad (forM_, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
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
import NeoNote.Note.Parse (parseNoteFilter)
import NeoNote.Search
import NeoNote.Store.Database (Database, runDatabase)
import NeoNote.Store.Note
import NeoNote.Time
import Optics.Core

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
  Action.ScanNotes -> runScanNotesAction

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

runListNotesAction :: (CLI :> es, Error NeoNoteError :> es, GetTime :> es) => Text -> [NoteAttribute] -> Int -> OrderBy NoteAttribute -> Eff es ()
runListNotesAction search noteAttributes showAmount orderBy = do
  noteFilter <- makeNoteFilter search
  displayNotes noteFilter orderBy showAmount noteAttributes

runScanNotesAction :: (IOE :> es) => Eff es ()
runScanNotesAction = liftIO $ putStrLn "Sorry, not yet implemented"

makeNoteFilter :: (GetTime :> es, Error NeoNoteError :> es) => Text -> Eff es NoteFilter
makeNoteFilter searchText = do
  time <- getCurrentTime
  case parseNoteFilter time searchText of
    Left err -> throwError (CannotParseFilter searchText err)
    Right noteFilter -> pure noteFilter
