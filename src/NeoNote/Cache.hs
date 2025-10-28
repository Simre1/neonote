module NeoNote.Cache where

import Control.Monad (when)
import Data.Hashable (hash)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Note.Note
import NeoNote.Time (GetTime, Time, addDays, formatTime, getCurrentTime, timeFromString)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory, removeDirectoryRecursive)
import System.FilePath (joinPath)

class Cacheable a where
  writeCacheItem :: FilePath -> a -> IO ()
  readCacheItem :: FilePath -> IO a

data Cache :: Effect where
  Cache :: (Cacheable a) => Text -> NoteInfo -> m a -> Cache m a

makeEffect ''Cache

runCache :: (GetConfiguration :> es, IOE :> es, GetTime :> es) => Eff (Cache : es) a -> Eff es a
runCache = interpret $ \env (Cache kind noteInfo computeItem) -> do
  notesPath <- getConfiguration #notesPath
  currentTime <- getCurrentTime
  cacheDir <- liftIO $ getXdgDirectory XdgCache "neonote"
  liftIO $ cleanCache currentTime cacheDir
  let prefix = hash notesPath
      modifiedString = T.replace " " "" $ formatTime $ noteInfo.modified
      idString = noteIdToText $ noteInfo.id
      fileName = [i|#{prefix}-#{idString}-#{modifiedString}-#{kind}|]
      filePath = joinPath [cacheDir, fileName]
  cachedHighlightExists <- liftIO $ doesFileExist filePath
  if cachedHighlightExists
    then do
      liftIO $ readCacheItem filePath
    else do
      item <- localSeqUnlift env $ \unlift -> unlift computeItem
      liftIO $ writeCacheItem filePath item
      pure item

cleanCache :: Time -> FilePath -> IO ()
cleanCache currentTime cacheDir = do
  lastCacheCleanFileExists <- doesFileExist lastCacheCleanFile
  lastCacheClean <-
    if lastCacheCleanFileExists
      then timeFromString <$> T.readFile lastCacheCleanFile
      else pure Nothing
  case lastCacheClean of
    Nothing -> cleanFiles
    Just lastTime ->
      when (addDays lastTime 14 < currentTime) cleanFiles
  where
    cleanFiles :: IO ()
    cleanFiles = do
      removeDirectoryRecursive cacheDir
      createDirectoryIfMissing True cacheDir
      T.writeFile lastCacheCleanFile $ formatTime currentTime
    lastCacheCleanFile :: FilePath
    lastCacheCleanFile = joinPath [cacheDir, "lastCacheClean"]

instance Cacheable Text where
  writeCacheItem = T.writeFile
  readCacheItem = T.readFile
