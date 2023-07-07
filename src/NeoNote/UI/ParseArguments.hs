module NeoNote.UI.ParseArguments where

import Options.Applicative hiding (action)
import NeoNote.Actions

parseActionFromArguments :: IO Action
parseActionFromArguments = execParser action

action :: ParserInfo Action
action = info (subparser createAction <|> subparser searchAction) (progDesc "Manage your notes with NeoNote")

createAction :: Mod CommandFields Action
createAction = command "create" $ info 
  (pure CreateNote) 
  (progDesc "Create a new note")
  
searchAction :: Mod CommandFields Action
searchAction = command "search" $ info 
  (pure SearchNote) 
  (progDesc "Search notes")