module Main (main) where

import FakeEnvironment
import Test.Tasty
import Test.Tasty.HUnit
import NeoNote.Log
import Optics.Core
import NeoNote.Run (handleAction)
import NeoNote.Actions qualified as Action
import NeoNote.Store.Note
import NeoNote.Note.Note
import Data.Set qualified as S (fromList)

main :: IO ()
main = defaultMain $
  testGroup "NeoNote" [
    testGroup "Create" [createNoNote, createSingleNote, createMultipleNotes],
    testGroup "Tag" [singleTag]
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


  where fakeData = FakeData {defaultExtension = "md", editorWrites = []}

createNoNote :: TestTree
createNoNote = testCase "No note" $ do 
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "")
    findNotes EveryNote
  
  fakeOutput ^. #logs @?= [NoteEmpty]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= []


  where fakeData = FakeData {defaultExtension = "md", editorWrites = []}
   
   
createMultipleNotes :: TestTree
createMultipleNotes = testCase "Multiple notes" $ do 
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test1")
    handleAction (Action.CreateNote False "")
    handleAction (Action.CreateNote False "test")
    noteIds <- findNotes EveryNote
    noteInfos <- traverse getNoteInfo noteIds
    S.fromList <$> traverse readNote noteInfos
  
  fakeOutput ^. #logs @?= [NoteCreated, NoteCreated, NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= S.fromList [NoteContent "test1", NoteContent "test2", NoteContent "test3"]

  where fakeData = FakeData {defaultExtension = "md", editorWrites = ["test2", "3"]}

singleTag :: TestTree
singleTag = testCase "Single tag" $ do 
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "#test")
    noteIds <- findNotes (HasTag (Tag "test"))
    noteInfos <- traverse getNoteInfo noteIds
    traverse readNote noteInfos
  
  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= [NoteContent "#test"]

  where fakeData = FakeData {defaultExtension = "md", editorWrites = []}
