module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Set qualified as S (fromList)
import Data.Text.IO qualified as T
import FakeEnvironment
import NeoNote.Actions qualified as Action
import NeoNote.Configuration
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Run (handleAction)
import NeoNote.Store.Note
import NeoNote.Time
import Optics.Core
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "NeoNote"
      [ testGroup "Create" [createNoNote, createSingleNote, createMultipleNotes, addMultipleNotes],
        testGroup "Edit" [editSingleNote],
        testGroup "Tag" [singleTag, hyphenatedTag, manyTags, updateTagAfterEdit],
        testGroup "Date" [after2020, modificationChangesDate]
      ]

createSingleNote :: TestTree
createSingleNote = testCase "Single note" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")
    noteIds <- findNotes EveryNote
    pure $ length noteIds

  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

createNoNote :: TestTree
createNoNote = testCase "No note" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "")
    findNotes EveryNote

  fakeOutput ^. #logs @?= [NoteEmpty]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= []
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

createMultipleNotes :: TestTree
createMultipleNotes = testCase "Multiple notes" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test1")
    handleAction (Action.CreateNote False "")
    handleAction (Action.CreateNote False "test")
    noteIds <- findNotes EveryNote
    notes <- traverse readNote noteIds
    pure $ S.fromList $ notes ^. mapping #content

  fakeOutput ^. #logs @?= [NoteCreated, NoteCreated, NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= S.fromList [NoteContent "test1", NoteContent "test2", NoteContent "test3"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = ["test2", "3"]}

addMultipleNotes :: TestTree
addMultipleNotes = testCase "Add multiple notes" $ do
  fakeOutput <- runFakeIO fakeData $ do
    notesPath <- getConfiguration #notesPath
    liftIO $ do
      T.writeFile (notesPath </> "1.md") "Note1"
      T.writeFile (notesPath </> "2.md") "---\nmy-tag\n---\nNote2"
      T.writeFile (notesPath </> "3.md") "Note3"
    handleAction (Action.AddNotes ((notesPath </>) <$> ["1.md", "2.md", "3.md"]))
    allNotes <- findNotes EveryNote >>= traverse readNote
    tagNote <- findNotes (HasField (FieldName "my-tag")) >>= traverse readNote
    pure $ (S.fromList $ allNotes ^. mapping #content, tagNote ^. mapping #content)

  fakeOutput ^. #logs @?= [NotesAdded]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right (allNotes, tagNote) -> do
      allNotes @?= S.fromList [NoteContent "Note1", NoteContent "Note2", NoteContent "Note3"]
      tagNote @?= [NoteContent "Note2"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = ["test2", "3"]}

editSingleNote :: TestTree
editSingleNote = testCase "Single note" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")
    handleAction (Action.EditNote 1 "")

    noteIds <- findNotes EveryNote
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  fakeOutput ^. #logs @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent "test123"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = ["123"]}

singleTag :: TestTree
singleTag = testCase "Single tag" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "---\ntest\n---")
    noteIds <- findNotes (HasField (FieldName "test"))
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent ""]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

hyphenatedTag :: TestTree
hyphenatedTag = testCase "Hyphenated tag" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "---\ntest-tag\n---")
    noteIds <- findNotes (HasField (FieldName "test-tag"))
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent ""]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

manyTags :: TestTree
manyTags = testCase "Many tags" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "---\none\n---")
    handleAction (Action.CreateNote True "---\none\ntwo\n---")
    handleAction (Action.CreateNote True "---\none\ntwo\nthree\n---")

    noteIdsOne <- findNotes (HasField (FieldName "one"))
    noteIdsOneTwo <- findNotes (And (HasField $ FieldName "one") (HasField $ FieldName "two"))
    noteIdsNoThree <- findNotes (Not $ HasField (FieldName "three"))

    pure $ length <$> [noteIdsOne, noteIdsOneTwo, noteIdsNoThree]

  fakeOutput ^. #logs @?= [NoteCreated, NoteCreated, NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [3, 2, 2]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

updateTagAfterEdit :: TestTree
updateTagAfterEdit = testCase "Update tag after edit" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "-")
    handleAction (Action.EditNote 1 "")

    noteIds <- findNotes (HasField (FieldName "test"))
    pure $ length noteIds

  fakeOutput ^. #logs @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = ["--\ntest\n---"]}

after2020 :: TestTree
after2020 = testCase "After 2020" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")

    noteIds <- findNotes (Check Greater (DateLiteral DateLiteralCreated) (DateLiteral $ DateLiteralTime (mempty & #year ?~ 2020)))

    pure $ length noteIds

  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

modificationChangesDate :: TestTree
modificationChangesDate = testCase "Modification changes date" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")

    unmodifiedNoteIds <- findNotes (Check Equal (DateLiteral DateLiteralCreated) (DateLiteral DateLiteralModified))
    unmodifiedNoteInfos <- traverse readNoteInfo unmodifiedNoteIds

    liftIO $ threadDelay 1000000

    mapM_ (`writeNote` RawNote "new content") unmodifiedNoteInfos

    modifiedNoteIds <- findNotes (Check Lesser (DateLiteral DateLiteralCreated) (DateLiteral DateLiteralModified))

    pure $ length <$> [unmodifiedNoteIds, modifiedNoteIds]

  fakeOutput ^. #logs @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [1, 1]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}
