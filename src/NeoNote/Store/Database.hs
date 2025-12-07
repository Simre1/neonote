module NeoNote.Store.Database where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Coerce
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (groupBy, intersperse)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as S
import Data.String.Interpolate (__i)
import Data.Text as T (Text, null, pack)
import Data.Traversable
import Database.SQLite.Simple qualified as DB
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localLiftUnlift, localSeqUnlift, localUnlift)
import Effectful.Error.Dynamic
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import NeoNote.Configuration
import NeoNote.Error
import NeoNote.Note.Note
import NeoNote.Note.Operations
import NeoNote.Store.Database.Error
import NeoNote.Time (IncompleteTime (..), Time, formatTime, timeFromString)
import Optics.Core
import System.FilePath (joinPath)

data Database :: Effect where
  DbWriteNoteInfo :: NoteInfo -> Database m ()
  DbWriteNotes :: [Note] -> Database m ()
  DbReadNote :: NoteId -> Database m Note
  DbReadNoteInfo :: NoteId -> Database m NoteInfo
  DbFindNotes :: NoteFilter -> Selection -> Database m [NoteId]
  DbNoteExists :: NoteId -> Database m Bool
  DbDeleteNote :: NoteId -> Database m ()
  DbStageEdit :: ((NoteId -> Time -> RawNote -> m ()) -> m a) -> Database m a
  DbCommitStagedEdits :: (NoteId -> Time -> RawNote -> m (Update Note)) -> Database m ()

data Selection = Selection
  { amount :: Int,
    order :: OrderBy NoteAttribute
  }
  deriving (Show, Eq, Ord, Generic)

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
          ( \env databaseEffect -> case databaseEffect of
              DbNoteExists noteId -> handleNoteExists connection noteId
              DbWriteNoteInfo noteInfo -> handleWriteNoteInfo connection noteInfo
              DbWriteNotes note -> handleWriteNote True connection note
              DbReadNote note -> handleReadNote connection note
              DbReadNoteInfo noteId -> handleReadNoteInfo connection noteId
              DbFindNotes notesFilter selection -> handleFindNotes connection notesFilter selection
              DbDeleteNote noteId -> handleDBDeleteNote True connection noteId
              DbStageEdit make -> localLiftUnlift env (ConcUnlift Persistent Unlimited) $ \lift unlift ->
                handleStageEdit connection (\kont -> unlift $ make (\a b c -> lift $ kont a b c))
              DbCommitStagedEdits makeNote -> localUnlift env (ConcUnlift Persistent Unlimited) $ \unlift ->
                handleCommitStagedEdits connection (fmap (fmap unlift) . makeNote)
          )
          (inject eff)

handleStageEdit :: (IOE :> es) => DB.Connection -> ((NoteId -> Time -> RawNote -> Eff es ()) -> Eff es a) -> Eff es a
handleStageEdit connection kont = do
  stagedNotes <- liftIO $ newIORef S.empty
  let stage noteId modificationTime (RawNote rawNote) = liftIO $ do
        DB.execute
          connection
          [__i|  
              insert or replace into openedits (noteId, modified, raw)
              values (?, ?, ?)
            |]
          (noteIdToText noteId, formatTime modificationTime, rawNote)
        atomicModifyIORef' stagedNotes (\s -> (S.insert noteId s, ()))
  a <- kont stage
  noteIds <- fmap S.toList $ liftIO $ readIORef stagedNotes
  liftIO $ do
    DB.executeMany
      connection
      [__i|  
        delete from openedits
        where noteId = ?
        |]
      (DB.Only . noteIdToText <$> noteIds)
  pure a

handleCommitStagedEdits :: (IOE :> es, Error DatabaseError :> es) => DB.Connection -> (NoteId -> Time -> RawNote -> Eff es (Update Note)) -> Eff es ()
handleCommitStagedEdits connection makeNote = withRunInIO $ \unlift -> liftIO $ DB.withTransaction connection $ do
  openEdits <-
    liftIO $
      DB.query
        connection
        [__i| select noteId, modified, raw from openedits|]
        ()

  scheduledUpdates <- for openEdits $ \(noteIdString, modifiedTimeString, rawNote) -> do
    noteId <- maybe (unlift $ throwError $ CorruptedNoteId noteIdString) pure $ noteIdFromText noteIdString
    modifiedTime <- maybe (unlift $ throwError $ InvalidDateFormat noteId modifiedTimeString) pure $ timeFromString modifiedTimeString
    update <- unlift $ makeNote noteId modifiedTime (RawNote rawNote)
    pure (noteId, update)

  let (writes, deletes) = partitionUpdates scheduledUpdates

  unlift $ handleWriteNote False connection (snd <$> writes)
  unlift $ traverse_ (handleDBDeleteNote False connection) deletes

  liftIO $
    DB.execute
      connection
      [__i| delete from openedits |]
      ()

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
noteFilterToCondition (HasProperty propertyName) = do
  param <- newParam $ coerce propertyName
  pure [__i| (exists ( select noteId from properties where id = noteId and property = #{param})) |]
noteFilterToCondition (Check comp (DateLiteral dl1) (DateLiteral dl2)) =
  dateLiteralToCondition comp (Left dl1) (Left dl2)
noteFilterToCondition (Check comp (DateLiteral dl1) (PropertyLiteral propertyName)) =
  dateLiteralToCondition comp (Left dl1) (Right propertyName)
noteFilterToCondition (Check comp (PropertyLiteral propertyName) (DateLiteral dl2)) =
  dateLiteralToCondition comp (Right propertyName) (Left dl2)
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
getNonDateLiteral (PropertyLiteral propertyName) = do
  param <- newParam (propertyName ^. #name)
  pure [__i| (select value from properties where noteId = id and property = #{param}) |]
getNonDateLiteral IdLiteral = pure [__i| id |]
getNonDateLiteral (StringLiteral txt) = do
  param <- newParam txt
  pure [__i| #{param} |]
getNonDateLiteral (IntLiteral n) = do
  param <- newParamInt n
  pure [__i| #{param} |]
getNonDateLiteral (DateLiteral _) = error "Cannot get date literal"

dateLiteralToCondition :: Comparison -> Either DateLiteral PropertyName -> Either DateLiteral PropertyName -> State [DB.NamedParam] Text
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
    makeTimePartCondition :: (Lens' IncompleteTime (Maybe Int), Text) -> Either DateLiteral PropertyName -> State [DB.NamedParam] (Maybe Text)
    makeTimePartCondition (timePartLens, timePartFormat) dateLiteral = case dateLiteral of
      Left DateLiteralCreated -> pure $ Just [__i| ltrim(strftime('#{timePartFormat}', created), '0')|]
      Left DateLiteralModified -> pure $ Just [__i|ltrim(strftime('#{timePartFormat}', modified), '0')|]
      Left (DateLiteralTime incompleteTime) -> pure $ (\a -> "'" <> a <> "'") . pack . show <$> incompleteTime ^. timePartLens
      Right propertyName -> do
        param <- newParam (propertyName ^. #name)
        pure $ Just [__i|ltrim(strftime('#{timePartFormat}', (select value from properties where noteId = id and property = #{param}) ), '0')|]
    timeParts :: [(Lens' IncompleteTime (Maybe Int), Text)]
    timeParts = [(#year, "%Y"), (#month, "%m"), (#day, "%d"), (#hour, "%H"), (#minute, "%M"), (#seconds, "%S")]

generateConditionSql :: NoteFilter -> (Text, [DB.NamedParam])
generateConditionSql noteFilter = runState (noteFilterToCondition noteFilter) []

handleNoteExists :: (IOE :> es, Error DatabaseError :> es) => DB.Connection -> NoteId -> Eff es Bool
handleNoteExists connection noteId = do
  results :: [DB.Only Text] <- liftIO $ DB.query connection [__i| select id from notes where id = ?|] (DB.Only $ noteIdToText noteId)
  when (length results > 1) $
    throwError (TooManyResults noteId)
  pure $ length results == 1

handleDBDeleteNote :: (IOE :> es) => Bool -> DB.Connection -> NoteId -> Eff es ()
handleDBDeleteNote transaction connection noteId =
  liftIO $ (if transaction then DB.withTransaction connection else id) $ do
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
        delete from properties
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
        delete from properties
        where noteId = ?
      |]
      (DB.Only $ noteIdToText (noteInfo ^. #id))
    insertProperties connection (noteInfo ^. #id) (noteInfo ^. #properties)

insertProperties :: DB.Connection -> NoteId -> Properties -> IO ()
insertProperties connection noteId properties = do
  forM_ (propertyValues properties) $ \(property, value) ->
    DB.execute
      connection
      [__i|
              insert into properties (noteId, property, value)
              values (?,?,?)
            |]
      ( noteIdToText noteId,
        coerce @_ @Text property,
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
  propertyResults <- readProperties connection noteId
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
            properties = propertyResults,
            created = created,
            modified = modified,
            extension = extension
          }
    _ -> throwError (TooManyResults noteId)

handleFindNotes :: (IOE :> es, Error DatabaseError :> es) => DB.Connection -> NoteFilter -> Selection -> Eff es [NoteId]
handleFindNotes connection notesFilter selection = do
  let (condition, params) = generateConditionSql notesFilter
      conditionSQL =
        if not (T.null condition)
          then "where " <> condition
          else ""
      sql = [__i| select id from notes #{conditionSQL} order by #{orderToSql (selection ^. #order)}, #{orderToSql (Descending AttributeModified)} limit #{selection ^. #amount} |]
  results <-
    liftIO $
      DB.queryNamed
        connection
        sql
        params
  forM results $ \(DB.Only matchedNoteId) ->
    maybe (throwError $ CorruptedNoteId matchedNoteId) pure $ noteIdFromText matchedNoteId
  where
    orderToSql :: OrderBy NoteAttribute -> Text
    orderToSql order = case order of
      Ascending attribute -> [__i| #{attributeToColumn attribute} asc|]
      Descending attribute -> [__i| #{attributeToColumn attribute} desc|]
    attributeToColumn :: NoteAttribute -> Text
    attributeToColumn = \case
      AttributeId -> "id"
      AttributeCreated -> "created"
      AttributeModified -> "modified"
      AttributeExtension -> "extension"
      AttributeProperties -> [__i| (group_concat(select property from properties where noteId = id order by property))|]

handleWriteNote :: (IOE :> es) => Bool -> DB.Connection -> [Note] -> Eff es ()
handleWriteNote transaction connection notes =
  liftIO $ (if transaction then DB.withTransaction connection else id) $ forM_ notes $ \(Note noteInfo (NoteContent noteContent)) -> do
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
        delete from openedits
        where noteId = ?
      |]
      (DB.Only $ noteIdToText (noteInfo ^. #id))
    DB.execute
      connection
      [__i|  
        delete from properties
        where noteId = ?
      |]
      (DB.Only $ noteIdToText (noteInfo ^. #id))
    insertProperties connection (noteInfo ^. #id) (noteInfo ^. #properties)

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
  propertyResults <- readProperties connection noteId
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
                  properties = propertyResults,
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
          pure $ Just $ IncompatibleTableVersion tableVersion currentVersion
    _ -> pure $ Just $ CorruptedTable "versions"
  where
    currentVersion :: Int
    currentVersion = 4
    migrate :: Int -> Int -> IO ()
    migrate n _
      | n < currentVersion = do
          putStrLn "Automatic migration is not possible."
          putStrLn "You need to upload all notes manually:\n1. Backup notes with nn export -o backup_folder\n2. Update neonote\n3. Delete notes folder\n4. nn import backup_folder/*"
    migrate n m = do
      putStrLn [__i| Table version #{n} is higher then neonote version #{m}" |]

readProperties :: (IOE :> es) => DB.Connection -> NoteId -> Eff es Properties
readProperties connection noteId = liftIO $ do
  propertyRows :: [(Text, DB.SQLData)] <-
    DB.query
      connection
      [__i| select property, value from properties where noteId = ? |]
      (DB.Only $ noteIdToText noteId)
  let propertyMap = M.fromListWith (++) $ fmap (first PropertyName . second pure) propertyRows
  pure $ Properties $ S.fromList . mapMaybe sqlToValue <$> propertyMap

comparisonToSql :: Comparison -> Text -> Text -> Text
comparisonToSql Equal v1 v2 = v1 <> " = " <> v2
comparisonToSql Greater v1 v2 = v1 <> " > " <> v2
comparisonToSql Lesser v1 v2 = v1 <> " < " <> v2
comparisonToSql GreaterEqual v1 v2 = v1 <> " >= " <> v2
comparisonToSql LesserEqual v1 v2 = v1 <> " <= " <> v2
comparisonToSql Similar v1 v2 = v1 <> " LIKE '%' || " <> v2 <> " || '%'"

valueToSql :: Value -> DB.SQLData
valueToSql (IntValue n) = DB.SQLInteger (fromIntegral n)
valueToSql (StringValue txt) = DB.SQLText txt
valueToSql NoValue = DB.SQLNull

sqlToValue :: DB.SQLData -> Maybe Value
sqlToValue (DB.SQLInteger n) = Just (IntValue (fromIntegral n))
sqlToValue (DB.SQLText txt) = Just (StringValue txt)
sqlToValue DB.SQLNull = Just NoValue
sqlToValue _ = Nothing

tables :: [DB.Query]
tables = ["PRAGMA encoding = \"UTF-8\"", noteTable, propertyTable, configTable, openEditsTable] ++ searchTable
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
    propertyTable :: DB.Query
    propertyTable =
      [__i|
        CREATE TABLE IF NOT EXISTS properties
          (noteId TEXT NOT NULL,
          property TEXT NOT NULL,
          value TEXT
          )
      |]
    openEditsTable :: DB.Query
    openEditsTable =
      [__i|
        CREATE TABLE IF NOT EXISTS openedits
          (noteId TEXT PRIMARY KEY NOT NULL,
           modified TEXT NOT NULL,
           raw TEXT
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
