module NeoNote.Store.Database where

import Control.Exception (catch)
import Control.Monad (forM, forM_, when)
import Data.Coerce
import Data.Set qualified as S
import Data.String.Interpolate (__i)
import Data.Text as T (Text, null)
import Database.SQLite.Simple qualified as DB
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Data.Note
import NeoNote.Error
import NeoNote.Store.Database.Error
import NeoNote.Store.Files
import NeoNote.Time (timeFromString, timeToString)
import Optics.Core

data Database :: Effect where
  GetNoteInfo :: NoteId -> Database m NoteInfo
  WriteNoteInfo :: NoteId -> NoteInfo -> Database m ()
  FindNotes :: NoteFilter -> Database m [NoteId]
  NoteExists :: NoteId -> Database m Bool

makeEffect ''Database

runDatabase :: forall es a es'. (es' ~ Error DatabaseError : es, IOE :> es, Files :> es, Error NeoNoteError :> es) => Eff (Database : es) a -> Eff es a
runDatabase eff = do
  dbPath <- getDatabasePath
  withRunInIO $ \unlift ->
    catch
      ( DB.withConnection dbPath $ \connection -> do
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
              )
              (inject eff)
      )
      $ \e -> unlift $ runDatabaseError $ throwError $ SQLiteDBCrashed e
  where
    noteFilterToCondition :: NoteFilter -> (Text, [DB.NamedParam])
    noteFilterToCondition (HasTag tag) =
      let tagText = coerce @_ @Text tag
       in ( [__i| (exists ( select noteId, tag from tags where id = noteId and tag = :::::::#{tagText})) |],
            [":::::::" <> tagText DB.:= tagText]
          )
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

    handleNoteExists :: DB.Connection -> NoteId -> Eff es' Bool
    handleNoteExists connection noteId = do
      results :: [DB.Only Text] <- liftIO $ DB.query connection [__i| select id from notes where id = ?|] (DB.Only $ noteIdToText noteId)
      when (length results > 1) $
        throwError (TooManyResults noteId)
      pure $ length results == 1

    handleWriteNoteInfo :: DB.Connection -> NoteId -> NoteInfo -> Eff es' ()
    handleWriteNoteInfo connection noteId noteInfo =
      liftIO $ DB.withTransaction connection $ do
        DB.execute
          connection
          [__i|  
            insert or replace into notes (id, extension, createdAt, modifiedAt) 
            values (?,?,?,?)
          |]
          ( noteIdToText noteId,
            noteInfo ^. #extension,
            timeToString $ noteInfo ^. #createdAt,
            timeToString $ noteInfo ^. #modifiedAt
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
              select extension, createdAt, modifiedAt from
                (select id, extension, createdAt, modifiedAt from notes
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
        [(extension, createdAtText, modifiedAtText)] -> do
          createdAt <-
            maybe (throwError $ InvalidDateFormat noteId createdAtText) pure $
              timeFromString createdAtText
          modifiedAt <-
            maybe (throwError $ InvalidDateFormat noteId modifiedAtText) pure $
              timeFromString modifiedAtText
          pure $
            NoteInfo
              { tags = S.fromList $ Tag . (\(DB.Only tagText) -> tagText) <$> resultsTags,
                createdAt = createdAt,
                modifiedAt = modifiedAt,
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
                (select id, createdAt, modifiedAt from notes
                #{conditionSQL})
            |]
            params
      forM results $ \(DB.Only matchedNoteId) ->
        maybe (throwError $ CorruptedNoteId matchedNoteId) pure $ noteIdFromText matchedNoteId

    checkTableVersion :: DB.Connection -> IO (Maybe DatabaseError)
    checkTableVersion connection = DB.withTransaction connection $ do
      versionResults <- DB.query_ connection [__i| select tableVersion from versions |]
      case versionResults of
        [] -> do
          DB.execute_ connection [__i| insert into versions (tableVersion ) values (#{currentVersion})|]
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
tables = [noteTable, tagTable, versionTable]
  where
    noteTable :: DB.Query
    noteTable =
      [__i|
        CREATE TABLE IF NOT EXISTS notes 
          (id TEXT PRIMARY KEY NOT NULL,
          extension TEXT NOT NULL,
          createdAt TEXT NOT NULL,
          modifiedAt TEXT NOT NULL
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
    versionTable :: DB.Query
    versionTable =
      [__i|
        CREATE TABLE IF NOT EXISTS versions
          (tableVersion integer NOT NULL)
      |]
