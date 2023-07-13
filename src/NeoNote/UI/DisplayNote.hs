module NeoNote.UI.DisplayNote where
import NeoNote.Note.Note
import Effectful
import Data.Text.IO qualified as T
import NeoNote.Note.Highlight (highlight)

displayNoteInTerminal :: IOE :> es => NoteInfo -> NoteContent -> Eff es ()
displayNoteInTerminal noteInfo noteContent = do
  liftIO $ T.putStrLn $ highlight noteInfo noteContent
  liftIO $ T.putStrLn ""
