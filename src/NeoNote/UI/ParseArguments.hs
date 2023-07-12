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
    (progDesc "Manage your notes with NeoNote in your terminal")

version :: Parser (a -> a)
version = infoOption (showVersion P.version) (short 'v' <> long "version" <> help "Show version")

programActions :: Time -> Parser Action
programActions time =
  hsubparser createAction
    <|> hsubparser (pickAction time)
    <|> hsubparser (editAction time)
    <|> hsubparser (viewAction time)
    <|> hsubparser (deleteAction time)
    <|> hsubparser (listAction time)
    <|> hsubparser scanAction

createAction :: Mod CommandFields Action
createAction =
  commandWithShortcut
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
  commandWithShortcut
    "edit"
    ( info
        ( EditNote
            <$> noteFilter time
            <*> noteAmount 1
            <*> searchText
        )
        (progDesc "Edit a note")
    )
    <> metavar "edit"

deleteAction :: Time -> Mod CommandFields Action
deleteAction time =
  commandWithShortcut
    "delete"
    ( info
        ( DeleteNote
            <$> noteFilter time
            <*> noteAmount 1
            <*> searchText
        )
        (progDesc "Delete a note")
    )
    <> metavar "delete"

viewAction :: Time -> Mod CommandFields Action
viewAction time =
  commandWithShortcut
    "view"
    ( info
        ( ViewNote
            <$> noteFilter time
            <*> noteAmount 1
            <*> searchText
        )
        (progDesc "View a note")
    )
    <> metavar "view"

pickAction :: Time -> Mod CommandFields Action
pickAction time =
  commandWithShortcut
    "pick"
    ( info
        ( PickNote
            <$> noteFilter time
            <*> searchText
        )
        (progDesc "Pick a note")
    )
    <> metavar "pick"

listAction :: Time -> Mod CommandFields Action
listAction time =
  commandWithShortcut
    "list"
    ( info
        ( ListNotes
            <$> noteFilter time
            <*> many
              ( option
                  noteAttribute
                  ( short 'a'
                      <> long "attribute"
                      <> help "Note attributes which are shown (id|created|modified|extension|tags). You can use '-a' multiple times"
                  )
              )
            <*> noteAmount 8
            <*> ( Ascending <$> option noteAttribute (long "ascending" <> help "Order notes by attribute in ascending manner")
                    <|> Descending <$> option noteAttribute (value AttributeModified <> long "descending" <> help "Order notes by attribute in descending manner")
                )
            <*> searchText
        )
        (progDesc "List notes")
    )
    <> metavar "list"

scanAction :: Mod CommandFields Action
scanAction = commandWithShortcut "scan" (info (pure ScanNotes) (progDesc "Scan notes and update database")) <> metavar "scan"

searchText :: Parser Text
searchText = strArgument (value "" <> help "Initial search text" <> metavar "Search text")

noteAmount :: Int -> Parser Int
noteAmount defaultAmount = option auto (value defaultAmount <> short 'n' <> long "number" <> help "Number of notes")

noteFilter :: Time -> Parser NoteFilter
noteFilter time = do
  option
    ( do
        filterString <- str
        either (fail . unpack) pure $ parseNoteFilter time filterString
    )
    (long "filter" <> short 'f' <> value EveryNote)

commandWithShortcut :: String -> ParserInfo a -> Mod CommandFields a
commandWithShortcut t a = command t a <> command [head t] a

noteAttribute :: ReadM NoteAttribute
noteAttribute =
  AttributeId <$ maybeReader (guard . (\s -> s == "id" || s == "i"))
    <|> AttributeCreated <$ maybeReader (guard . (\s -> s == "created" || s == "c"))
    <|> AttributeModified <$ maybeReader (guard . (\s -> s == "modified" || s == "m"))
    <|> AttributeExtension <$ maybeReader (guard . (\s -> s == "extension" || s == "e"))
    <|> AttributeTags <$ maybeReader (guard . (\s -> s == "tags" || s == "t"))
