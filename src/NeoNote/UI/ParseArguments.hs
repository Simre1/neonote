{-# LANGUAGE ApplicativeDo #-}

module NeoNote.UI.ParseArguments where

import Control.Monad (guard)
import Data.Text (Text, unpack)
import Data.Version (showVersion)
import NeoNote.Actions
import NeoNote.Note.Note
import NeoNote.Note.Parse (parseNoteFilter)
import NeoNote.Time
import Options.Applicative hiding (action)
import Paths_neonote qualified as P (version)

parseActionFromArguments :: Time -> IO Action
parseActionFromArguments time = execParser (cliParser time)

cliParser :: Time -> ParserInfo Action
cliParser time =
  info
    (helper <*> version <*> programActions time)
    (progDesc "Manage your notes with NeoNote")

version :: Parser (a -> a)
version = infoOption (showVersion P.version) (short 'v' <> long "version" <> help "Show version")

programActions :: Time -> Parser Action
programActions time =
  hsubparser createAction
    <|> hsubparser (editAction time)
    <|> hsubparser (viewAction time)
    <|> hsubparser (deleteAction time)
    <|> hsubparser (listAction time)
    <|> hsubparser scanAction

createAction :: Mod CommandFields Action
createAction =
  command
    "create"
    ( info
        ( CreateNote
            <$> switch (long "skip-editor" <> short 's' <> help "Skip the editor and save the note")
            <*> strArgument (value "" <> help "Text for the note" <> metavar "Note text")
        )
        (progDesc "Create a new note")
    )
    <> metavar "create"

editAction :: Time -> Mod CommandFields Action
editAction time =
  command
    "edit"
    ( info
        ( EditNote
            <$> noteFilter time
            <*> switch (long "skip-picker" <> short 's' <> help "Skip the picker and edit the first note")
            <*> searchText
        )
        (progDesc "Edit a note")
    )
    <> metavar "edit"

deleteAction :: Time -> Mod CommandFields Action
deleteAction time =
  command
    "delete"
    ( info
        ( DeleteNote
            <$> noteFilter time
            <*> switch (long "skip-picker" <> short 's' <> help "Skip the picker and delete the first note")
            <*> searchText
        )
        (progDesc "Delete a note")
    )
    <> metavar "delete"

viewAction :: Time -> Mod CommandFields Action
viewAction time =
  command
    "view"
    ( info
        ( ViewNote
            <$> noteFilter time
            <*> switch (long "skip-picker" <> short 's' <> help "Skip the picker and view the first note")
            <*> searchText
        )
        (progDesc "View a note")
    )
    <> metavar "view"

listAction :: Time -> Mod CommandFields Action
listAction time =
  command
    "list"
    ( info
        ( ListNotes
            <$> noteFilter time
            <*> many (option noteAttribute (short 'a' <> long "attribute" <> help "Note attributes which are shown (id|created|modified|extension|tags). You can use '-a' multiple times"))
            <*> option auto (value 20 <> long "amount" <> help "Amount of notes to list")
            <*> ( Ascending <$> option noteAttribute (long "ascending" <> help "Order notes by attribute in ascending manner")
                    <|> Descending <$> option noteAttribute (value AttributeId <> long "descending" <> help "Order notes by attribute in descending manner")
                )
            <*> searchText
        )
        (progDesc "List notes")
    )
    <> metavar "list"

scanAction :: Mod CommandFields Action
scanAction = command "scan" (info (pure ScanNotes) (progDesc "Scan notes and update database")) <> metavar "scan"

searchText :: Parser Text
searchText = strArgument (value "" <> help "Initial search text" <> metavar "Search text")

noteFilter :: Time -> Parser NoteFilter
noteFilter time = do
  option
    ( do
        filterString <- str
        either (fail . unpack) pure $ parseNoteFilter time filterString
    )
    (long "filter" <> short 'f' <> value EveryNote)

noteAttribute :: ReadM NoteAttribute
noteAttribute =
  AttributeId <$ maybeReader (guard . (\s -> s == "id" || s == "i"))
    <|> AttributeCreated <$ maybeReader (guard . (\s -> s == "created" || s == "c"))
    <|> AttributeModified <$ maybeReader (guard . (\s -> s == "modified" || s == "m"))
    <|> AttributeExtension <$ maybeReader (guard . (\s -> s == "extension" || s == "e"))
    <|> AttributeTags <$ maybeReader (guard . (\s -> s == "tags" || s == "t"))
