module NeoNote.Store.Database where

import Control.Monad (forM, forM_, when)
import Data.Bifunctor (bimap)
import Data.Coerce
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.String.Interpolate (__i)
import Data.Text as T (Text, null, pack)
import Database.SQLite.Simple qualified as DB
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Error
import NeoNote.Note.Note
import NeoNote.Store.Database.Error
import NeoNote.Store.Files
import NeoNote.Time (IncompleteTime (..), timeFromString, timeToString)
import Optics.Core

data Database :: Effect where
  GetNoteInfo :: NoteId -> Database m NoteInfo
  WriteNoteInfo :: NoteId -> NoteInfo -> Database m ()
  FindNotes :: NoteFilter -> Database m [NoteId]
  NoteExists :: NoteId -> Database m Bool
  DBDeleteNote :: NoteId -> Database m ()

makeEffect ''Database

runDatabase :: forall es a es'. (es' ~ Error DatabaseError : es, IOE :> es, Files :> es, Error NeoNoteError :> es) => Eff (Database : es) a -> Eff es a
runDatabase eff = do
  dbPath <- getDatabasePath
  withRunInIO $ \unlift ->
    DB.withConnection dbPath $ \connection -> do
      mapM_ (DB.execute_ connection) tables
      maybeTableVersionError <- checkTableVersion connection
      unlift $ runDatabaseError $ do
        maybe (pure ()) throwError maybeTableVersionError
        interpret
          ( \_ databaseEffect -> case databaseEffect of
              NoteExists noteId -> handleNoteExists connection noteId
              WriteNoteInfo noteId noteInfo -> handleWriteNoteInfo connection noteId noteInfo
              GetNoteInfo noteId -> handleGetNoteInfo connection noteId
              FindNotes notesFilter -> handleFindNotes connection notesFilter
              DBDeleteNote noteId -> handleDBDeleteNote connection noteId
          )
          (inject eff)
  where
    noteFilterToCondition :: NoteFilter -> (Text, [DB.NamedParam])
    noteFilterToCondition (HasTag tag) =
      let tagText = coerce @_ @Text tag
       in ( [__i| (exists ( select noteId, tag from tags where id = noteId and tag = :::::::#{tagText})) |],
            [":::::::" <> tagText DB.:= tagText]
          )
    --
    noteFilterToCondition (EqualDate d1 d2) = (dateLiteralToCondition "=" d1 d2, [])
    noteFilterToCondition (AfterDate d1 d2) = (dateLiteralToCondition ">" d1 d2, [])
    noteFilterToCondition (BeforeDate d1 d2) = (dateLiteralToCondition "<" d1 d2, [])
    --
    noteFilterToCondition (And filter1 filter2) =
      let (condition1, params1) = noteFilterToCondition filter1
          (condition2, params2) = noteFilterToCondition filter2
       in ([__i| (#{condition1} and #{condition2}) |], params1 ++ params2)
    noteFilterToCondition (Or filter1 filter2) =
      let (condition1, params1) = noteFilterToCondition filter1
          (condition2, params2) = noteFilterToCondition filter2
       in ([__i| (#{condition1} or #{condition2}) |], params1 ++ params2)
    noteFilterToCondition (Not filter1) =
      let (condition1, params1) = noteFilterToCondition filter1
       in ([__i| (not #{condition1}) |], params1)
    noteFilterToCondition EveryNote = ("1=1", [])
    dateLiteralToCondition :: Text -> DateLiteral -> DateLiteral -> Text
    dateLiteralToCondition operation d1 d2 =
      let sqlConcat atoms = mconcat $ intersperse " || " atoms
          (c1, c2) = bimap sqlConcat sqlConcat $ unzip $ mapMaybe (\tp -> (,) <$> makeTimePartCondition tp d1 <*> makeTimePartCondition tp d2) timeParts
       in if not (T.null c1)
            then c1 <> operation <> c2
            else "1=1"
      where
        makeTimePartCondition :: (Lens' IncompleteTime (Maybe Int), Text) -> DateLiteral -> Maybe Text
        makeTimePartCondition (timePartLens, timePartFormat) dateLiteral = case dateLiteral of
          DateLiteralCreated -> Just [__i| ltrim(strftime('#{timePartFormat}', created), '0')|]
          DateLiteralModified -> Just [__i|ltrim(strftime('#{timePartFormat}', modified), '0')|]
          DateLiteral incompleteTime -> (\a -> "'" <> a <> "'") . pack . show <$> incompleteTime ^. timePartLens
        timeParts :: [(Lens' IncompleteTime (Maybe Int), Text)]
        timeParts = [(#year, "%Y"), (#month, "%m"), (#day, "%d"), (#hour, "%H"), (#minute, "%M"), (#seconds, "%S")]

    handleNoteExists :: DB.Connection -> NoteId -> Eff es' Bool
    handleNoteExists connection noteId = do
      results :: [DB.Only Text] <- liftIO $ DB.query connection [__i| select id from notes where id = ?|] (DB.Only $ noteIdToText noteId)
      when (length results > 1) $
        throwError (TooManyResults noteId)
      pure $ length results == 1

    handleDBDeleteNote :: DB.Connection -> NoteId -> Eff es' ()
    handleDBDeleteNote connection noteId =
      liftIO $ DB.withTransaction connection $ do
        DB.execute
          connection
          [__i|  
            delete from notes
            where id = ?
          |]
          (DB.Only $ noteIdToText noteId)
        DB.execute
          connection
          [__i|  
            delete from tags
            where noteId = ?
          |]
          (DB.Only $ noteIdToText noteId)

    handleWriteNoteInfo :: DB.Connection -> NoteId -> NoteInfo -> Eff es' ()
    handleWriteNoteInfo connection noteId noteInfo =
      liftIO $ DB.withTransaction connection $ do
        DB.execute
          connection
          [__i|  
            insert or replace into notes (id, extension, created, modified) 
            values (?,?,?,?)
          |]
          ( noteIdToText noteId,
            noteInfo ^. #extension,
            timeToString $ noteInfo ^. #created,
            timeToString $ noteInfo ^. #modified
          )
        DB.execute
          connection
          [__i|  
            delete from tags
            where noteId = ?
          |]
          (DB.Only $ noteIdToText noteId)
        forM_ (noteInfo ^. #tags) $ \tag ->
          DB.execute
            connection
            [__i|
              insert into tags (noteId, tag)
              values (?,?)
            |]
            (noteIdToText noteId, coerce @_ @Text tag)

    handleGetNoteInfo :: DB.Connection -> NoteId -> Eff es' NoteInfo
    handleGetNoteInfo connection noteId = do
      resultsNotes <-
        liftIO $
          DB.query
            connection
            [__i|
              select extension, created, modified from
                (select id, extension, created, modified from notes
                where id = ?)
            |]
            (DB.Only $ noteIdToText noteId)
      resultsTags <-
        liftIO $
          DB.query
            connection
            [__i|
              select tag from
                (select noteId, tag from tags
                where noteId = ?)
            |]
            (DB.Only $ noteIdToText noteId)
      case resultsNotes of
        [] -> throwError (MissingNoteId noteId)
        [(extension, createdText, modifiedText)] -> do
          created <-
            maybe (throwError $ InvalidDateFormat noteId createdText) pure $
              timeFromString createdText
          modified <-
            maybe (throwError $ InvalidDateFormat noteId modifiedText) pure $
              timeFromString modifiedText
          pure $
            NoteInfo
              { tags = S.fromList $ Tag . (\(DB.Only tagText) -> tagText) <$> resultsTags,
                created = created,
                modified = modified,
                extension = extension
              }
        _ -> throwError (TooManyResults noteId)

    handleFindNotes :: DB.Connection -> NoteFilter -> Eff es' [NoteId]
    handleFindNotes connection notesFilter = do
      let (condition, params) = noteFilterToCondition notesFilter
          conditionSQL =
            if not (T.null condition)
              then "where " <> condition
              else ""
      results <-
        liftIO $
          DB.queryNamed
            connection
            [__i|
              select id from 
                (select id, created, modified from notes
                #{conditionSQL})
            |]
            params
      forM results $ \(DB.Only matchedNoteId) ->
        maybe (throwError $ CorruptedNoteId matchedNoteId) pure $ noteIdFromText matchedNoteId

    checkTableVersion :: DB.Connection -> IO (Maybe DatabaseError)
    checkTableVersion connection = DB.withTransaction connection $ do
      versionResults <- DB.query_ connection [__i| select tableVersion from config|]
      case versionResults of
        [] -> do
          DB.execute_ connection [__i| insert into config (tableVersion) values (#{currentVersion})|]
          pure Nothing
        [DB.Only tableVersion] ->
          if tableVersion == currentVersion
            then pure Nothing
            else pure $ Just $ IncompatibleTableVersion currentVersion tableVersion
        _ -> pure $ Just $ CorruptedTable "versions"
      where
        currentVersion :: Int
        currentVersion = 1

tables :: [DB.Query]
tables = [noteTable, tagTable, configTable]
  where
    noteTable :: DB.Query
    noteTable =
      [__i|
        CREATE TABLE IF NOT EXISTS notes 
          (id TEXT PRIMARY KEY NOT NULL,
          extension TEXT NOT NULL,
          created TEXT NOT NULL,
          modified TEXT NOT NULL
          )
      |]
    tagTable :: DB.Query
    tagTable =
      [__i|
        CREATE TABLE IF NOT EXISTS tags
          (noteId TEXT NOT NULL,
          tag TEXT NOT NULL
          )
      |]
    configTable :: DB.Query
    configTable =
      [__i|
        CREATE TABLE IF NOT EXISTS config
          (tableVersion integer NOT NULL)
      |]
