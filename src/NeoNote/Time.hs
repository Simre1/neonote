module NeoNote.Time where

import Control.Applicative (Alternative ((<|>)))
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Data.Time qualified as Time
import Data.Time.Clock (addUTCTime, secondsToNominalDiffTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Optics.Core

newtype Time = Time UTCTime deriving (Generic, Show, Eq, Ord)

formatTime :: Time -> Text
formatTime (Time utcTime) =
  pack $
    Time.formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime

formatDate :: Time -> Text
formatDate (Time utcTime) =
  pack $
    Time.formatTime defaultTimeLocale "%Y-%m-%d" utcTime

formatTimestamp :: Text -> Time -> Text
formatTimestamp prefix (Time utcTime) =
  prefix <> "-" <> pack (Time.formatTime defaultTimeLocale "%Y-%m-%d" utcTime)

timeFromString :: Text -> Maybe Time
timeFromString text =
  Time
    <$> parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack text)

-- timeOfDayToString :: TimeOfDay -> Text
-- timeOfDayToString (TimeOfDay timeOfDay) =
--   pack $ formatTime defaultTimeLocale "%H:%M:%S" timeOfDay

-- timeOfDayFromString :: Text -> Maybe TimeOfDay
-- timeOfDayFromString text =
--   TimeOfDay
--     <$> parseTimeM False defaultTimeLocale "%H:%M:%S" (unpack text)

-- dayToString :: Day -> Text
-- dayToString (Day day) =
--   pack $ formatTime defaultTimeLocale "%H:%M:%S" day

dayFromString :: Text -> Maybe IncompleteTime
dayFromString text = do
  (y, m, d) <- Time.toGregorian <$> parseTimeM False defaultTimeLocale "%Y-%m-%d" (unpack text)
  pure $
    mempty
      & #year
      ?~ fromIntegral y
      & #month
      ?~ m
      & #day
      ?~ fromEnum d

timeOfDayFromString :: Text -> Maybe IncompleteTime
timeOfDayFromString text = do
  (Time.TimeOfDay h m s) <- parseTimeM False defaultTimeLocale "%H:%M:%S" (unpack text)
  pure $
    mempty
      & #hour
      ?~ h
      & #minute
      ?~ m
      & #seconds
      ?~ fromEnum s

addSeconds :: Time -> Int -> Time
addSeconds (Time t) seconds = Time $ addUTCTime (secondsToNominalDiffTime $ fromIntegral seconds) t

addMinutes :: Time -> Int -> Time
addMinutes t m = addSeconds t (m * 60)

addHours :: Time -> Int -> Time
addHours t hours = addMinutes t (hours * 60)

addDays :: Time -> Int -> Time
addDays t days = addMinutes t (days * 24)

-- timeToTimeOfDay :: Time -> TimeOfDay
-- timeToTimeOfDay time =
--   fromJust $ timeOfDayFromString $ T.drop 11 $ timeToString time

-- timeToDay :: Time -> Day
-- timeToDay time =
--   fromJust $ dayFromString $ T.take 10 $ timeToString time

data IncompleteTime = IncompleteTime
  { year :: Maybe Int,
    month :: Maybe Int,
    day :: Maybe Int,
    hour :: Maybe Int,
    minute :: Maybe Int,
    seconds :: Maybe Int
  }
  deriving (Generic, Show, Eq, Ord)

instance Semigroup IncompleteTime where
  it1 <> it2 =
    IncompleteTime
      { year = f #year,
        month = f #month,
        day = f #day,
        hour = f #hour,
        minute = f #minute,
        seconds = f #seconds
      }
    where
      f l = it1 ^. l <|> it2 ^. l

instance Monoid IncompleteTime where
  mempty = IncompleteTime Nothing Nothing Nothing Nothing Nothing Nothing

data GetTime :: Effect where
  GetCurrentTime :: GetTime m Time

makeEffect ''GetTime

runGetTime :: (IOE :> es) => Eff (GetTime : es) a -> Eff es a
runGetTime = interpret $ \_ GetCurrentTime -> Time <$> liftIO Time.getCurrentTime
