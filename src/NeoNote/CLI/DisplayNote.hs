module NeoNote.CLI.DisplayNote where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Text.IO qualified as T
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.IO qualified as TL
import Effectful
import NeoNote.Note.Highlight (Highlight, highlight)
import NeoNote.Note.Note
import NeoNote.Note.Syntax.RawNote (makeFrontmatter)
import Optics.Core

displayNoteInTerminal :: (IOE :> es, Highlight :> es) => Bool -> Bool -> Note -> Eff es ()
displayNoteInTerminal plain frontmatter (Note noteInfo noteContent) = do
  content <-
    if not plain
      then highlight noteInfo noteContent
      else pure $ coerce noteContent
  liftIO $ do
    when frontmatter $ TL.putStr $ B.toLazyText (makeFrontmatter (noteInfo ^. #fields))
    T.putStrLn content
