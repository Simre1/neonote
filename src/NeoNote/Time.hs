module NeoNote.Time where
import GHC.Generics (Generic)
import Data.Text (Text, pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, parseTimeM, UTCTime)
import Data.Time qualified as Time (getCurrentTime)
import Effectful
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (interpret)

newtype Time = Time UTCTime deriving (Generic, Show, Eq, Ord)

timeToString :: Time -> Text
timeToString (Time utcTime) =
  pack $
    formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime

timeFromString :: Text -> Maybe Time
timeFromString text =
  Time
    <$> parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack text)


data GetTime :: Effect where
  GetCurrentTime :: GetTime m Time

makeEffect ''GetTime

runGetTime :: (IOE :> es) => Eff (GetTime : es) a -> Eff es a
runGetTime = interpret $ \_ GetCurrentTime -> Time <$> liftIO Time.getCurrentTime