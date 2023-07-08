{-# LANGUAGE ApplicativeDo #-}

module NeoNote.UI.ParseArguments where

import Data.Text (unpack)
import NeoNote.Actions
import NeoNote.Note.Note
import Options.Applicative hiding (action)
import NeoNote.Note.Parse (parseNoteFilter)

parseActionFromArguments :: IO Action
parseActionFromArguments = execParser action

action :: ParserInfo Action
action = info (subparser createAction <|> subparser searchAction) (progDesc "Manage your notes with NeoNote")

createAction :: Mod CommandFields Action
createAction =
  command "create" $
    info
      (pure CreateNote)
      (progDesc "Create a new note")

searchAction :: Mod CommandFields Action
searchAction =
  command "search" $
    info
      (SearchNote <$> noteFilter)
      (progDesc "Search notes")

noteFilter :: Parser NoteFilter
noteFilter = do
  option
    ( do
        filterString <- str
        either (fail . unpack) pure $ parseNoteFilter filterString
    )
    (long "filter" <> short 'f' <> value EveryNote)
