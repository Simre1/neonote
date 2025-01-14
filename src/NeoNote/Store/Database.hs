module NeoNote.Store.Database where

import Control.Monad (forM, forM_, when)
import Control.Monad.Trans.State
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
import NeoNote.Configuration
import NeoNote.Error
import NeoNote.Note.Note
import NeoNote.Store.Database.Error
import NeoNote.Time (IncompleteTime (..), timeFromString, timeToString)
import Optics.Core
import System.FilePath (joinPath)

data Database :: Effect where
  DbWriteNoteInfo :: NoteInfo -> Database m ()
  DbWriteNotes :: [Note] -> Database m ()
  DbReadNote :: NoteId -> Database m Note
  DbReadNoteInfo :: NoteId -> Database m NoteInfo
  DbFindNotes :: NoteFilter -> Database m [NoteId]
  DbNoteExists :: NoteId -> Database m Bool
  DbDeleteNote :: NoteId -> Database m ()

makeEffect ''Database

dbWriteNote :: (Database :> es) => Note -> Eff es ()
dbWriteNote x = dbWriteNotes [x]

runDatabase :: forall es a es'. (es' ~ Error DatabaseError : es, IOE :> es, GetConfiguration :> es, Error NeoNoteError :> es) => Eff (Database : es) a -> Eff es a
runDatabase eff = do
  dbPath <- makeDatabasePath <$> getConfiguration #notesPath
  withRunInIO $ \unlift ->
    DB.withConnection dbPath $ \connection -> do
      mapM_ (DB.execute_ connection) tables
      maybeTableVersionError <- checkTableVersion connection
      unlift $ runDatabaseError $ do
        maybe (pure ()) throwError maybeTableVersionError
        interpret
          ( \_ databaseEffect -> case databaseEffect of
              DbNoteExists noteId -> handleNoteExists connection noteId
              DbWriteNoteInfo noteInfo -> handleWriteNoteInfo connection noteInfo
              DbWriteNotes note -> handleWriteNote connection note
              DbReadNote note -> handleReadNote connection note
              DbReadNoteInfo noteId -> handleReadNoteInfo connection noteId
              DbFindNotes notesFilter -> handleFindNotes connection notesFilter
              DbDeleteNote noteId -> handleDBDeleteNote connection noteId
          )
          (inject eff)
  where
    newParam :: Text -> State [DB.NamedParam] Text
    newParam input = do
      params <- get
      let paramName = pack $ (":namedparam" ++) $ show $ length params
      modify ((paramName DB.:= input) :)
      pure paramName

    noteFilterToCondition :: NoteFilter -> State [DB.NamedParam] Text
    noteFilterToCondition (HasTag tag) = do
      param <- newParam $ coerce tag
      pure [__i| (exists ( select noteId, tag from tags where id = noteId and tag = #{param})) |]
    --
    noteFilterToCondition (EqualDate d1 d2) = pure $ dateLiteralToCondition "=" d1 d2
    noteFilterToCondition (AfterDate d1 d2) = pure $ dateLiteralToCondition ">" d1 d2
    noteFilterToCondition (BeforeDate d1 d2) = pure $ dateLiteralToCondition "<" d1 d2
    --
    noteFilterToCondition (Together filter1 filter2) =
      noteFilterToCondition (And filter1 filter2)
    noteFilterToCondition (And filter1 filter2) = do
      condition1 <- noteFilterToCondition filter1
      condition2 <- noteFilterToCondition filter2
      pure [__i| (#{condition1} and #{condition2}) |]
    noteFilterToCondition (Or filter1 filter2) = do
      condition1 <- noteFilterToCondition filter1
      condition2 <- noteFilterToCondition filter2
      pure [__i| (#{condition1} or #{condition2}) |]
    noteFilterToCondition (Not filter1) = do
      condition <- noteFilterToCondition filter1
      pure [__i| (not #{condition}) |]
    noteFilterToCondition EveryNote = pure "1=1"
    noteFilterToCondition (Contains fragment) = do
      param <- newParam ("\"" <> fragment <> "\"")
      pure [__i| (exists ( select noteId, content from notes_search where id = noteId and content match concat(#{param}, '*'))) |]
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

    handleWriteNoteInfo :: DB.Connection -> NoteInfo -> Eff es' ()
    handleWriteNoteInfo connection noteInfo =
      liftIO $ DB.withTransaction connection $ do
        DB.execute
          connection
          [__i|  
            insert or replace into notes (id, extension, created, modified) 
            values (?,?,?,?)
          |]
          ( noteIdToText (noteInfo ^. #id),
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
          (DB.Only $ noteIdToText (noteInfo ^. #id))
        forM_ (noteInfo ^. #tags) $ \tag ->
          DB.execute
            connection
            [__i|
              insert into tags (noteId, tag)
              values (?,?)
            |]
            (noteIdToText (noteInfo ^. #id), coerce @_ @Text tag)

    handleReadNoteInfo :: DB.Connection -> NoteId -> Eff es' NoteInfo
    handleReadNoteInfo connection noteId = do
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
              { id = noteId,
                tags = S.fromList $ Tag . (\(DB.Only tagText) -> tagText) <$> resultsTags,
                created = created,
                modified = modified,
                extension = extension
              }
        _ -> throwError (TooManyResults noteId)

    handleFindNotes :: DB.Connection -> NoteFilter -> Eff es' [NoteId]
    handleFindNotes connection notesFilter = do
      let (condition, params) = runState (noteFilterToCondition notesFilter) []
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

    handleWriteNote :: DB.Connection -> [Note] -> Eff es' ()
    handleWriteNote connection notes =
      liftIO $ DB.withTransaction connection $ forM_ notes $ \(Note noteInfo (NoteContent noteContent)) -> do
        DB.execute
          connection
          [__i|  
            insert or replace into notes (id, extension, created, modified, content) 
            values (?,?,?,?,?)
          |]
          ( noteIdToText (noteInfo ^. #id),
            noteInfo ^. #extension,
            timeToString $ noteInfo ^. #created,
            timeToString $ noteInfo ^. #modified,
            noteContent
          )
        DB.execute
          connection
          [__i|  
            delete from tags
            where noteId = ?
          |]
          (DB.Only $ noteIdToText (noteInfo ^. #id))
        forM_ (noteInfo ^. #tags) $ \tag ->
          DB.execute
            connection
            [__i|
              insert into tags (noteId, tag)
              values (?,?)
            |]
            (noteIdToText (noteInfo ^. #id), coerce @_ @Text tag)

    handleReadNote :: DB.Connection -> NoteId -> Eff es' Note
    handleReadNote connection noteId = do
      resultsNotes <-
        liftIO $
          DB.query
            connection
            [__i|
              select extension, created, modified, content from
                (select id, extension, created, modified, content from notes
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
        [(extension, createdText, modifiedText, noteContent)] -> do
          created <-
            maybe (throwError $ InvalidDateFormat noteId createdText) pure $
              timeFromString createdText
          modified <-
            maybe (throwError $ InvalidDateFormat noteId modifiedText) pure $
              timeFromString modifiedText
          pure $
            Note
              { info =
                  NoteInfo
                    { id = noteId,
                      tags = S.fromList $ Tag . (\(DB.Only tagText) -> tagText) <$> resultsTags,
                      created = created,
                      modified = modified,
                      extension = extension
                    },
                content = NoteContent noteContent
              }
        _ -> throwError (TooManyResults noteId)

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
            else do
              migrate tableVersion currentVersion
              pure $ Just $ IncompatibleTableVersion currentVersion tableVersion
        _ -> pure $ Just $ CorruptedTable "versions"
      where
        currentVersion :: Int
        currentVersion = 2
        migrate :: Int -> Int -> IO ()
        migrate 1 2 = do
          putStrLn "Automatic migration not yet possible."
          putStrLn "You need to upload all notes manually:\n1. Backup notes\n2. Delete notes folder\n3. Within notes backup: nn upload *"
        migrate _ _ = error "Weird versions"

tables :: [DB.Query]
tables = ["PRAGMA encoding = \"UTF-8\"", noteTable, tagTable, configTable] ++ searchTable
  where
    noteTable :: DB.Query
    noteTable =
      [__i|
        CREATE TABLE IF NOT EXISTS notes 
          (id TEXT PRIMARY KEY NOT NULL,
          extension TEXT NOT NULL,
          created TEXT NOT NULL,
          modified TEXT NOT NULL,
          content TEXT
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
    searchTable :: [DB.Query]
    searchTable =
      [ [__i|
        CREATE VIRTUAL TABLE IF NOT EXISTS notes_search
        USING fts5(
          content,
          noteId UNINDEXED
        );|],
        [__i|
        CREATE TRIGGER IF NOT EXISTS notes_search_insert_trigger
        AFTER INSERT ON notes
        BEGIN
          INSERT INTO notes_search (content, noteId)
          VALUES (NEW.content, NEW.id);
        END;|],
        [__i|
        CREATE TRIGGER IF NOT EXISTS notes_search_update_trigger
        AFTER UPDATE ON notes
        BEGIN
          UPDATE notes_search
          SET content = NEW.content
          WHERE noteId = NEW.id;
        END;|],
        [__i|
        CREATE TRIGGER IF NOT EXISTS notes_search_delete_trigger
        AFTER DELETE ON notes
        BEGIN
          DELETE FROM notes_search
          WHERE noteId = OLD.id;
        END;                 
      |]
      ]

makeDatabasePath :: FilePath -> FilePath
makeDatabasePath notesPath = joinPath [notesPath, "notes.db"]
