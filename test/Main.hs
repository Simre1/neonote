module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Set qualified as S (fromList)
import FakeEnvironment
import NeoNote.Actions qualified as Action
import NeoNote.Log
import NeoNote.Note.Note
import NeoNote.Run (handleAction)
import NeoNote.Store.Note
import NeoNote.Time
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "NeoNote"
      [ testGroup "Create" [createNoNote, createSingleNote, createMultipleNotes],
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
    handleAction (Action.CreateNote True "#test")
    noteIds <- findNotes (HasTag (Tag "test"))
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent "#test"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

hyphenatedTag :: TestTree
hyphenatedTag = testCase "Hyphenated tag" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "#test-tag")
    noteIds <- findNotes (HasTag (Tag "test-tag"))
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent "#test-tag"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

manyTags :: TestTree
manyTags = testCase "Many tags" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "#one")
    handleAction (Action.CreateNote True "#one #two")
    handleAction (Action.CreateNote True "#one #two #three")

    noteIdsOne <- findNotes (HasTag (Tag "one"))
    noteIdsOneTwo <- findNotes (And (HasTag $ Tag "one") (HasTag $ Tag "two"))
    noteIdsNoThree <- findNotes (Not $ HasTag (Tag "three"))

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
    handleAction (Action.CreateNote True "no tag")
    handleAction (Action.EditNote 1 "")

    noteIds <- findNotes (HasTag (Tag "test"))
    pure $ length noteIds

  fakeOutput ^. #logs @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = [" #test"]}

after2020 :: TestTree
after2020 = testCase "After 2020" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")

    noteIds <- findNotes (AfterDate DateLiteralCreated (DateLiteral (mempty & #year ?~ 2020)))

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

    unmodifiedNoteIds <- findNotes (EqualDate DateLiteralCreated DateLiteralModified)
    unmodifiedNoteInfos <- traverse readNoteInfo unmodifiedNoteIds

    liftIO $ threadDelay 1000000

    mapM_ (`writeNote` NoteContent "new content") unmodifiedNoteInfos

    modifiedNoteIds <- findNotes (BeforeDate DateLiteralCreated DateLiteralModified)

    pure $ length <$> [unmodifiedNoteIds, modifiedNoteIds]

  fakeOutput ^. #logs @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [1, 1]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}
