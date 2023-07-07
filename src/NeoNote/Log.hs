module NeoNote.Log where
import Effectful
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (interpret)

data Message

data Warning

data Log :: Effect where
  LogMessage :: Message -> Log m ()
  LogWarning :: Warning -> Log m ()

makeEffect ''Log

runLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runLog = interpret $ \_ -> \case
  LogMessage _ -> liftIO $ putStrLn "Message"
  LogWarning _ -> liftIO $ putStrLn "Warning"
