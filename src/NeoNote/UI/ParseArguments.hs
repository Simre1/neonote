{-# LANGUAGE ApplicativeDo #-}

module NeoNote.UI.ParseArguments where

import Data.Text (unpack)
import NeoNote.Actions
import NeoNote.Note.Note
import Options.Applicative hiding (action)
import NeoNote.Note.Parse (parseNoteFilter)
import NeoNote.Time

parseActionFromArguments :: Time -> IO Action
parseActionFromArguments time = execParser (action time)

action :: Time -> ParserInfo Action
action time = info (subparser createAction <|> subparser (searchAction time)) (progDesc "Manage your notes with NeoNote")

createAction :: Mod CommandFields Action
createAction =
  command "create" $
    info
      (pure CreateNote)
      (progDesc "Create a new note")

searchAction :: Time -> Mod CommandFields Action
searchAction time =
  command "search" $
    info
      (SearchNote <$> noteFilter time)
      (progDesc "Search notes")

noteFilter :: Time -> Parser NoteFilter
noteFilter time = do
  option
    ( do
        filterString <- str
        either (fail . unpack) pure $ parseNoteFilter time filterString
    )
    (long "filter" <> short 'f' <> value EveryNote)
