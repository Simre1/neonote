{-# LANGUAGE ApplicativeDo #-}

module NeoNote.CLI.ParseArguments where

import Control.Monad (guard)
import Data.Text (Text, unpack)
import Data.Version (showVersion)
import NeoNote.Actions
import NeoNote.Note.Note
import NeoNote.Note.Parse (parseNoteFilter)
import NeoNote.Time
import Options.Applicative hiding (action)
import Paths_neonote qualified as P (version)

parseActionFromArguments :: IO Action
parseActionFromArguments = execParser cliParser

cliParser :: ParserInfo Action
cliParser =
  info
    (helper <*> version <*> programActions)
    (progDesc "Manage your notes with NeoNote in your terminal")

version :: Parser (a -> a)
version = infoOption (showVersion P.version) (short 'v' <> long "version" <> help "Show version")

programActions :: Parser Action
programActions =
  hsubparser createAction
    <|> hsubparser pickAction
    <|> hsubparser editAction
    <|> hsubparser viewAction
    <|> hsubparser deleteAction
    <|> hsubparser listAction
    <|> hsubparser addNotesAction
    <|> hsubparser exportNotesAction

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

editAction :: Mod CommandFields Action
editAction =
  commandWithShortcut
    "edit"
    ( info
        ( EditNote
            <$> noteAmount 1
            <*> searchText
        )
        (progDesc "Filter notes and then edit them")
    )
    <> metavar "edit"

deleteAction :: Mod CommandFields Action
deleteAction =
  commandWithShortcut
    "delete"
    ( info
        ( DeleteNote
            <$> noteAmount 1
            <*> searchText
        )
        (progDesc "Filter notes and then delete them")
    )
    <> metavar "delete"

viewAction :: Mod CommandFields Action
viewAction =
  commandWithShortcut
    "view"
    ( info
        ( ViewNote
            <$> noteAmount 1
            <*> flag False True (long "plain" <> short 'p' <> help "Get plain output with no highlighting")
            <*> searchText
        )
        (progDesc "Filter notes and then view them")
    )
    <> metavar "view"

pickAction :: Mod CommandFields Action
pickAction =
  commandWithShortcut
    "pick"
    ( info
        ( PickNote
            <$> searchText
        )
        (progDesc "Pick notes to edit/view/delete")
    )
    <> metavar "pick"

listAction :: Mod CommandFields Action
listAction =
  commandWithShortcut
    "list"
    ( info
        ( ListNotes
            <$> many
              ( option
                  noteAttribute
                  ( short 'a'
                      <> long "attribute"
                      <> help "Note attributes which are shown (id|created|modified|extension|tags). You can use '-a' multiple times"
                  )
              )
            <*> noteAmount 8
            <*> ( Ascending
                    <$> option noteAttribute (long "ascending" <> help "Order notes by attribute in ascending manner")
                      <|> Descending
                    <$> option noteAttribute (value AttributeModified <> long "descending" <> help "Order notes by attribute in descending manner")
                )
            <*> searchText
        )
        (progDesc "Filter notes and list them")
    )
    <> metavar "list"

addNotesAction :: Mod CommandFields Action
addNotesAction = command "import" (info (AddNotes <$> paths) (progDesc "Import notes from filepaths")) <> metavar "import"

exportNotesAction :: Mod CommandFields Action
exportNotesAction = command "export" (info (ExportNotes <$> exportPath <*> searchText) (progDesc "Export notes into a folder")) <> metavar "export"

paths :: Parser [FilePath]
paths = many $ strArgument (help "Note path" <> metavar "Note paths")

exportPath :: Parser (Maybe FilePath)
exportPath = optional $ strOption (short 'o' <> long "output" <> help "Export directory")

searchText :: Parser Text
searchText = strArgument (value "" <> help "Search text" <> metavar "Search text")

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
  AttributeId
    <$ maybeReader (guard . (\s -> s == "id" || s == "i"))
      <|> AttributeCreated
    <$ maybeReader (guard . (\s -> s == "created" || s == "c"))
      <|> AttributeModified
    <$ maybeReader (guard . (\s -> s == "modified" || s == "m"))
      <|> AttributeExtension
    <$ maybeReader (guard . (\s -> s == "extension" || s == "e"))
      <|> AttributeTags
    <$ maybeReader (guard . (\s -> s == "tags" || s == "t"))
