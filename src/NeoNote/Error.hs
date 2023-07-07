module NeoNote.Error where
import Control.Exception (SomeException)
import Effectful
import Effectful.Error.Dynamic
import NeoNote.Store.Database.Error

data NeoNoteError = 
  EditingCrashed SomeException |
  SearchUICrashed SomeException |
  DatabaseError DatabaseError CallStack deriving (Show)

runDatabaseError :: (Error NeoNoteError :> es) => Eff (Error DatabaseError : es) a -> Eff es a
runDatabaseError eff = do
  result <- runError eff
  case result of
    Right a -> pure a
    Left (callstack, err) -> throwError (DatabaseError  err callstack)

runNeoNoteError ::
  (IOE :> es) =>
  Eff (Error NeoNoteError : es) () ->
  Eff es ()
runNeoNoteError eff = do
  result <- runError eff
  case result of
    Right _ -> pure ()
    Left (callstack, err) -> case err of
      DatabaseError dbError dbCallStack -> printError dbCallStack dbError
      _ -> printError callstack err
  where 
    printError callstack err = 
      liftIO $ do
        print err
        putStrLn $ prettyCallStack callstack
        pure ()
