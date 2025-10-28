module NeoNote.Data.Id where

import Control.Monad (guard, replicateM)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text as T (Text, all, length, pack)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import System.Random (randomRIO)

newtype Id = Id Text deriving (Show, Eq, Ord, Generic)

idLength :: Int
idLength = 6

idAlphabet :: S.Set Char
idAlphabet = S.fromList $ ['0' .. '9'] ++ ['a' .. 'z']

idAlphabetMapping :: M.Map Int Char
idAlphabetMapping = M.fromList $ zip [0 ..] (S.toList idAlphabet)

idToText :: Id -> Text
idToText (Id t) = t

looksLikeId :: Text -> Bool
looksLikeId = isJust . parseId

parseId :: Text -> Maybe Id
parseId text = do
  guard $ T.length text == 6
  guard $ T.all (`S.member` idAlphabet) text
  pure $ Id text

makeIdIO :: IO Id
makeIdIO = do
  indices <- replicateM 6 $ randomRIO (minIndex, maxIndex)
  let chars = (idAlphabetMapping M.!) <$> indices
  pure $ Id $ pack chars
  where
    minIndex = 0
    maxIndex = S.size idAlphabet - 1

data MakeId :: Effect where
  MakeId :: MakeId m Id

makeEffect ''MakeId

runMakeId :: (IOE :> es) => Eff (MakeId : es) a -> Eff es a
runMakeId = interpret $ \_ MakeId -> liftIO makeIdIO
