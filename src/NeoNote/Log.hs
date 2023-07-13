module NeoNote.Log where

import Data.Text (Text)
import Data.Text.IO qualified as T (putStrLn)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)

data Message
  = NoMatchingNote
  | NoteCreated
  | NoteEmpty
  | NoteEdited
  | NoteUnchanged
  deriving (Generic, Show, Eq, Ord)

data Warning

data Log :: Effect where
  LogMessage :: Message -> Log m ()
  LogWarning :: Warning -> Log m ()

makeEffect ''Log

showMessage :: Message -> Text
showMessage NoMatchingNote = "No notes match your query"
showMessage NoteCreated = "Note has been created"
showMessage NoteEmpty = "Note is empty, so nothing has been done"
showMessage NoteEdited = "Note has been edited"
showMessage NoteUnchanged = "Note is unchanged, so nothing has been done"

runLog :: (IOE :> es) => Eff (Log : es) a -> Eff es a
runLog = interpret $ \_ -> \case
  LogMessage message -> liftIO $ T.putStrLn (showMessage message)
  LogWarning _ -> liftIO $ putStrLn "Warning"
