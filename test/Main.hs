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

main :: IO ()
main = defaultMain $
  createNoteTest
  
createNoteTest :: TestTree
createNoteTest = testCase "create note" $ do 
  fakeOutput <- runFakeIO fakeData $ do
    handleAction (Action.CreateNote True "test")
    noteIds <- findNotes EveryNote
    pure $ length noteIds
  
  fakeOutput ^. #logs @?= [NoteCreated]
  case fakeOutput ^. #output of
    Left _ -> assertFailure "Unexpected NeoNoteError"
    Right a -> a @?= 1


  where fakeData = FakeData {defaultExtension = "md", editorWrites = []}
   
