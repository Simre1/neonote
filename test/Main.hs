module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (mapMaybe)
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

defaultSelection :: Selection
defaultSelection = Selection 100 (Ascending AttributeModified)

getNoteMessages :: FakeOutput a -> [NoteMessage]
getNoteMessages fakeOutput = mapMaybe (\case (NoteMessage _ nm) -> pure nm; _ -> Nothing) (fakeOutput ^. #logs)

createSingleNote :: TestTree
createSingleNote = testCase "Single note" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")
    noteIds <- findNotes EveryNote defaultSelection
    pure $ length noteIds

  getNoteMessages fakeOutput @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

createNoNote :: TestTree
createNoNote = testCase "No note" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "")
    findNotes EveryNote defaultSelection

  getNoteMessages fakeOutput @?= [NoteEmpty]
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
    noteIds <- findNotes EveryNote defaultSelection
    notes <- traverse readNote noteIds
    pure $ S.fromList $ notes ^. mapping #content

  getNoteMessages fakeOutput @?= [NoteCreated, NoteCreated, NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= S.fromList [NoteContent "test1", NoteContent "test2", NoteContent "test3"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = [Append "test2", Append "3"]}

addMultipleNotes :: TestTree
addMultipleNotes = testCase "Add multiple notes" $ do
  fakeOutput <- runFakeIO fakeData $ do
    notesPath <- getConfiguration #notesPath
    liftIO $ do
      T.writeFile (notesPath </> "1.md") "Note1"
      T.writeFile (notesPath </> "2.md") "---\nmy-tag\n---\nNote2"
      T.writeFile (notesPath </> "3.md") "Note3"
    handleAction (Action.AddNotes ((notesPath </>) <$> ["1.md", "2.md", "3.md"]))
    allNotes <- findNotes EveryNote defaultSelection >>= traverse readNote
    tagNote <- findNotes (HasProperty (PropertyName "my-tag")) defaultSelection >>= traverse readNote
    pure $ (S.fromList $ allNotes ^. mapping #content, tagNote ^. mapping #content)

  getNoteMessages fakeOutput @?= [NoteCreated, NoteCreated, NoteCreated]
  last (fakeOutput ^. #logs) @?= NotesAdded
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right (allNotes, tagNote) -> do
      allNotes @?= S.fromList [NoteContent "Note1", NoteContent "Note2", NoteContent "Note3"]
      tagNote @?= [NoteContent "Note2"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = [Append "test2", Append "3"]}

editSingleNote :: TestTree
editSingleNote = testCase "Single note" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")
    handleAction (Action.EditNote 1 "")

    noteIds <- findNotes EveryNote defaultSelection
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  getNoteMessages fakeOutput @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent "test123"]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = [Append "123"]}

singleTag :: TestTree
singleTag = testCase "Single tag" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "---\ntest\n---")
    noteIds <- findNotes (HasProperty (PropertyName "test")) defaultSelection
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  getNoteMessages fakeOutput @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent ""]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

hyphenatedTag :: TestTree
hyphenatedTag = testCase "Hyphenated tag" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "---\ntest-tag\n---")
    noteIds <- findNotes (HasProperty (PropertyName "test-tag")) defaultSelection
    notes <- traverse readNote noteIds
    pure $ notes ^. mapping #content

  getNoteMessages fakeOutput @?= [NoteCreated]
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

    noteIdsOne <- findNotes (HasProperty (PropertyName "one")) defaultSelection
    noteIdsOneTwo <- findNotes (And (HasProperty $ PropertyName "one") (HasProperty $ PropertyName "two")) defaultSelection
    noteIdsNoThree <- findNotes (Not $ HasProperty (PropertyName "three")) defaultSelection

    pure $ length <$> [noteIdsOne, noteIdsOneTwo, noteIdsNoThree]

  getNoteMessages fakeOutput @?= [NoteCreated, NoteCreated, NoteCreated]
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

    noteIds <- findNotes (HasProperty (PropertyName "test")) defaultSelection
    pure $ length noteIds

  getNoteMessages fakeOutput @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = [Append "--\ntest\n---"]}

after2020 :: TestTree
after2020 = testCase "After 2020" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")

    noteIds <- findNotes (Check Greater (DateLiteral DateLiteralCreated) (DateLiteral $ DateLiteralTime (mempty & #year ?~ 2020))) defaultSelection

    pure $ length noteIds

  getNoteMessages fakeOutput @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}

modificationChangesDate :: TestTree
modificationChangesDate = testCase "Modification changes date" $ do
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")

    unmodifiedNoteIds <- findNotes (Check Equal (DateLiteral DateLiteralCreated) (DateLiteral DateLiteralModified)) defaultSelection
    unmodifiedNotes <- traverse readNote unmodifiedNoteIds

    liftIO $ threadDelay 1000000

    mapM_ (`writeNote` RawNote "new content") unmodifiedNotes

    modifiedNoteIds <- findNotes (Check Lesser (DateLiteral DateLiteralCreated) (DateLiteral DateLiteralModified)) defaultSelection

    pure $ length <$> [unmodifiedNoteIds, modifiedNoteIds]

  getNoteMessages fakeOutput @?= [NoteCreated, NoteEdited]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [1, 1]
  where
    fakeData = FakeData {defaultExtension = "md", editorWrites = []}
