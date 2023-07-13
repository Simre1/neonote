module NeoNote.UI.DisplayNote where

import Data.Coerce (coerce)
import Data.Text.IO qualified as T
import Effectful
import NeoNote.Note.Highlight (Highlight, highlight)
import NeoNote.Note.Note

displayNoteInTerminal :: (IOE :> es, Highlight :> es) => Bool -> NoteInfo -> NoteContent -> Eff es ()
displayNoteInTerminal plain noteInfo noteContent = do
  content <-
    if not plain
      then highlight noteInfo noteContent
      else pure $ coerce noteContent
  liftIO $ T.putStrLn content
  liftIO $ T.putStrLn ""
