module NeoNote.Error where

import Control.Exception (SomeException)
import Data.String.Interpolate (__i)
import Data.Text
import Data.Text.IO qualified as T
import Effectful
import Effectful.Error.Dynamic
import GHC.Generics (Generic)
import NeoNote.Store.Database.Error

data NeoNoteError
  = CannotParseFilter Text Text
  | EditingCrashed SomeException
  | PickerCrashed SomeException
  | FileAccessFailed SomeException
  | DatabaseError DatabaseError CallStack
  deriving (Show, Generic)

runDatabaseError :: (Error NeoNoteError :> es) => Eff (Error DatabaseError : es) a -> Eff es a
runDatabaseError eff = do
  result <- runError eff
  case result of
    Right a -> pure a
    Left (callstack, err) -> throwError (DatabaseError err callstack)

runNeoNoteError ::
  (IOE :> es) =>
  Eff (Error NeoNoteError : es) () ->
  Eff es ()
runNeoNoteError eff = do
  result <- runError eff
  case result of
    Right _ -> pure ()
    Left (callstack, err) -> liftIO $ do
      let callstack' = case err of
            DatabaseError _ dbCallstack -> dbCallstack
            _ -> callstack
      T.putStrLn $ prettyNeoNoteError err
      putStrLn $ prettyCallStack callstack'
      pure ()
  where
    -- DatabaseError dbError dbCallStack -> printError dbCallStack (prettyDatabaseError dbError)
    -- _ -> printError callstack (prettyNeoNoteError err)

    prettyNeoNoteError :: NeoNoteError -> Text
    prettyNeoNoteError = \case
      CannotParseFilter searchText parseError -> [__i| Could not parse #{searchText} due to:\n#{parseError} |]
      EditingCrashed err -> [__i| The editor crashed due to #{err} |]
      PickerCrashed err -> [__i| The picker crashed due to #{err} |]
      FileAccessFailed err -> [__i| I could not access the file system due to #{err} |]
      DatabaseError dbError _ -> prettyDatabaseError dbError
