module NeoNote.Store.Database where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Coerce
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.String.Interpolate (__i)
import Data.Text as T (Text, null, pack)
import Data.Traversable
import Database.SQLite.Simple qualified as DB
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import NeoNote.Configuration
import NeoNote.Error
import NeoNote.Note.Note
import NeoNote.Store.Database.Error
import NeoNote.Time (IncompleteTime (..), formatTime, timeFromString)
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

newParam :: Text -> State [DB.NamedParam] Text
newParam input = do
  params <- get
  let paramName = pack $ (":namedparam" ++) $ show $ length params
  modify ((paramName DB.:= input) :)
  pure paramName

newParamInt :: Int -> State [DB.NamedParam] Text
newParamInt input = do
  params <- get
  let paramName = pack $ (":namedparam" ++) $ show $ length params
  modify ((paramName DB.:= input) :)
  pure paramName

noteFilterToCondition :: NoteFilter -> State [DB.NamedParam] Text
noteFilterToCondition (HasField fieldName) = do
  param <- newParam $ coerce fieldName
  pure [__i| (exists ( select noteId, field from fields where id = noteId and field = #{param})) |]
noteFilterToCondition (Check comp (DateLiteral dl1) (DateLiteral dl2)) =
  dateLiteralToCondition comp (Left dl1) (Left dl2)
noteFilterToCondition (Check comp (DateLiteral dl1) (FieldLiteral fieldName)) =
  dateLiteralToCondition comp (Left dl1) (Right fieldName)
noteFilterToCondition (Check comp (FieldLiteral fieldName) (DateLiteral dl2)) =
  dateLiteralToCondition comp (Right fieldName) (Left dl2)
noteFilterToCondition (Check _ (DateLiteral _) _) = pure "(1=1)"
noteFilterToCondition (Check _ _ (DateLiteral _)) = pure "(1=1)"
noteFilterToCondition (Check comp l1 l2) = nonDateLiteralToCondition comp l1 l2
-- noteFilterToCondition (HasId id) = do
--   param <- newParam $ idToText id
--   pure [__i| (id = #{param}) |]
--
-- noteFilterToCondition (EqualDate d1 d2) = pure $ dateLiteralToCondition "=" d1 d2
-- noteFilterToCondition (AfterDate d1 d2) = pure $ dateLiteralToCondition ">" d1 d2
-- noteFilterToCondition (BeforeDate d1 d2) = pure $ dateLiteralToCondition "<" d1 d2
--
noteFilterToCondition (Not filter1) = do
  condition <- noteFilterToCondition filter1
  pure [__i| (not #{condition}) |]
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
noteFilterToCondition (Contains fragment) = do
  param <- newParam ("\"" <> fragment <> "\"")
  pure [__i| (exists ( select noteId, content from notes_search where noteId = id and content match concat(#{param}, '*'))) |]
noteFilterToCondition EveryNote = pure "1=1"

nonDateLiteralToCondition :: Comparison -> Literal -> Literal -> State [DB.NamedParam] Text
nonDateLiteralToCondition comp l1 l2 = do
  v1 <- getNonDateLiteral l1
  v2 <- getNonDateLiteral l2
  pure [__i| (#{comparisonToSql comp v1 v2}) |]

getNonDateLiteral :: Literal -> State [DB.NamedParam] Text
getNonDateLiteral (FieldLiteral fieldName) = do
  param <- newParam (fieldName ^. #name)
  pure [__i| (select value from fields where noteId = id and field = #{param}) |]
getNonDateLiteral IdLiteral = pure [__i| id |]
getNonDateLiteral (StringLiteral txt) = do
  param <- newParam txt
  pure [__i| #{param} |]
getNonDateLiteral (IntLiteral n) = do
  param <- newParamInt n
  pure [__i| #{param} |]
getNonDateLiteral (DateLiteral _) = error "Cannot get date literal"

dateLiteralToCondition :: Comparison -> Either DateLiteral FieldName -> Either DateLiteral FieldName -> State [DB.NamedParam] Text
dateLiteralToCondition comp d1 d2 = do
  let sqlConcat atoms = mconcat $ intersperse " || " atoms
  timePartConditions <- forM timeParts $ \tp -> do
    c1 <- makeTimePartCondition tp d1
    c2 <- makeTimePartCondition tp d2
    pure $ (,) <$> c1 <*> c2
  let (c1, c2) = bimap sqlConcat sqlConcat $ unzip $ catMaybes timePartConditions
  pure $
    if not (T.null c1)
      then comparisonToSql comp c1 c2
      else "1=1"
  where
    makeTimePartCondition :: (Lens' IncompleteTime (Maybe Int), Text) -> Either DateLiteral FieldName -> State [DB.NamedParam] (Maybe Text)
    makeTimePartCondition (timePartLens, timePartFormat) dateLiteral = case dateLiteral of
      Left DateLiteralCreated -> pure $ Just [__i| ltrim(strftime('#{timePartFormat}', created), '0')|]
      Left DateLiteralModified -> pure $ Just [__i|ltrim(strftime('#{timePartFormat}', modified), '0')|]
      Left (DateLiteralTime incompleteTime) -> pure $ (\a -> "'" <> a <> "'") . pack . show <$> incompleteTime ^. timePartLens
      Right fieldName -> do
        param <- newParam (fieldName ^. #name)
        pure $ Just [__i|ltrim(strftime('#{timePartFormat}', (select value from fields where noteId = id and field = #{param}) ), '0')|]
    timeParts :: [(Lens' IncompleteTime (Maybe Int), Text)]
    timeParts = [(#year, "%Y"), (#month, "%m"), (#day, "%d"), (#hour, "%H"), (#minute, "%M"), (#seconds, "%S")]

generateConditionSql :: NoteFilter -> (Text, [DB.NamedParam])
generateConditionSql filter = runState (noteFilterToCondition filter) []

handleNoteExists :: (IOE :> es, Error DatabaseError :> es) => DB.Connection -> NoteId -> Eff es Bool
handleNoteExists connection noteId = do
  results :: [DB.Only Text] <- liftIO $ DB.query connection [__i| select id from notes where id = ?|] (DB.Only $ noteIdToText noteId)
  when (length results > 1) $
    throwError (TooManyResults noteId)
  pure $ length results == 1

handleDBDeleteNote :: (IOE :> es) => DB.Connection -> NoteId -> Eff es ()
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
        delete from fields
        where noteId = ?
      |]
      (DB.Only $ noteIdToText noteId)

handleWriteNoteInfo :: (IOE :> es) => DB.Connection -> NoteInfo -> Eff es ()
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
        formatTime $ noteInfo ^. #created,
        formatTime $ noteInfo ^. #modified
      )
    DB.execute
      connection
      [__i|  
        delete from fields
        where noteId = ?
      |]
      (DB.Only $ noteIdToText (noteInfo ^. #id))
    forM_ (M.toList $ noteInfo ^. #fields % #kv) $ \(field, value) ->
      DB.execute
        connection
        [__i|
          insert into fields (noteId, field, value)
          values (?,?,?)
        |]
        ( noteIdToText (noteInfo ^. #id),
          coerce @_ @Text field,
          valueToSql value
        )

handleReadNoteInfo :: (IOE :> es, Error DatabaseError :> es) => DB.Connection -> NoteId -> Eff es NoteInfo
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
  resultFields <- readFields connection noteId
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
            fields = resultFields,
            created = created,
            modified = modified,
            extension = extension
          }
    _ -> throwError (TooManyResults noteId)

handleFindNotes :: (IOE :> es, Error DatabaseError :> es) => DB.Connection -> NoteFilter -> Eff es [NoteId]
handleFindNotes connection notesFilter = do
  let (condition, params) = generateConditionSql notesFilter
      conditionSQL =
        if not (T.null condition)
          then "where " <> condition
          else ""
  results <-
    liftIO $
      DB.queryNamed
        connection
        [__i| select id from notes #{conditionSQL} |]
        params
  forM results $ \(DB.Only matchedNoteId) ->
    maybe (throwError $ CorruptedNoteId matchedNoteId) pure $ noteIdFromText matchedNoteId

handleWriteNote :: (IOE :> es) => DB.Connection -> [Note] -> Eff es ()
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
        formatTime $ noteInfo ^. #created,
        formatTime $ noteInfo ^. #modified,
        noteContent
      )
    DB.execute
      connection
      [__i|  
        delete from fields
        where noteId = ?
      |]
      (DB.Only $ noteIdToText (noteInfo ^. #id))
    forM_ (M.toList $ noteInfo ^. #fields % #kv) $ \(field, value) ->
      DB.execute
        connection
        [__i|
          insert into fields (noteId, field, value)
          values (?,?,?)
        |]
        ( noteIdToText (noteInfo ^. #id),
          coerce @_ @Text field,
          valueToSql value
        )

handleReadNote :: (IOE :> es, Error DatabaseError :> es) => DB.Connection -> NoteId -> Eff es Note
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
  resultFields <- readFields connection noteId
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
                  fields = resultFields,
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
    currentVersion = 3
    migrate :: Int -> Int -> IO ()
    migrate n _
      | n == 1 || n == 2 = do
          putStrLn "Automatic migration is not possible."
          putStrLn "You need to upload all notes manually:\n1. Backup notes with nn export -o backup_folder\n2. Update neonote\n3. Delete notes folder\n4. nn import backup_folder/*"
    migrate _ _ = error "Weird versions"

readFields :: (IOE :> es) => DB.Connection -> NoteId -> Eff es Fields
readFields connection noteId = liftIO $ do
  fieldRows :: [(Text, DB.SQLData)] <-
    DB.query
      connection
      [__i|
        select field, value from
          (select noteId, field, value from fields
          where noteId = ?)
      |]
      (DB.Only $ noteIdToText noteId)
  pure $ Fields $ M.fromList $ fmap (first FieldName . second sqlToValue) fieldRows

-- readFields :: (Traversable f) => DB.Connection -> f NoteId -> IO (f Fields)
-- readFields connection noteIds =
--   liftIO $ do
--     fieldRows :: [(Text, DB.SQLData)] <-
--       DB.query
--         connection
--         [__i|
--         select field, value from
--           (select noteId, field, value from fields
--           where noteId = ?)
--       |]
--         (noteIdToText <$> toList noteIds)
--     pure $ flip evalState fieldRows $ for noteIds $ \_ -> do
--       s <- get
--       case s of
--         (fields : rest) -> do
--           put rest
--           pure $ Fields $ M.fromList $ fmap (first FieldName . second sqlToValue) fields
--         [] -> error "readFields failed, not enough results"

comparisonToSql :: Comparison -> Text -> Text -> Text
comparisonToSql Equal v1 v2 = v1 <> " = " <> v2
comparisonToSql Greater v1 v2 = v1 <> " > " <> v2
comparisonToSql Lesser v1 v2 = v1 <> " < " <> v2
comparisonToSql GreaterEqual v1 v2 = v1 <> " >= " <> v2
comparisonToSql LesserEqual v1 v2 = v1 <> " <= " <> v2
comparisonToSql Similar v1 v2 = v1 <> " LIKE '%' || " <> v2 <> " || '%'"

valueToSql :: Value -> DB.SQLData
valueToSql NoValue = DB.SQLNull
valueToSql (IntValue n) = DB.SQLInteger (fromIntegral n)
valueToSql (StringValue txt) = DB.SQLText txt

sqlToValue :: DB.SQLData -> Value
sqlToValue (DB.SQLInteger n) = IntValue (fromIntegral n)
sqlToValue (DB.SQLText txt) = StringValue txt
sqlToValue _ = NoValue

tables :: [DB.Query]
tables = ["PRAGMA encoding = \"UTF-8\"", noteTable, fieldTable, configTable] ++ searchTable
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
    fieldTable :: DB.Query
    fieldTable =
      [__i|
        CREATE TABLE IF NOT EXISTS fields
          (noteId TEXT NOT NULL,
          field TEXT NOT NULL,
          value TEXT
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
