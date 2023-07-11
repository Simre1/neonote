module NeoNote.UI.DisplayNote where
import NeoNote.Note.Note
import Effectful
import Data.Text.IO qualified as T
import Data.Coerce (coerce)

displayNoteInTerminal :: IOE :> es => NoteInfo -> NoteContent -> Eff es ()
displayNoteInTerminal _noteInfo noteContent = do
  liftIO $ T.putStrLn $ coerce noteContent
