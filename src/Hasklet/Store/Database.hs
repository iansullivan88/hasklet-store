{-# LANGUAGE OverloadedStrings #-}

module Hasklet.Store.Database(
    createDatabaseActions,
    createSchema) where

import           Hasklet.Store.Transform
import           Hasklet.Store.Types
import           Prelude                          hiding (id)

import qualified Data.Aeson
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Pool
import qualified Data.Text                        as T
import           Data.Time.Clock
import           Data.UUID
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Types

newtype PersistUUID = PersistUUID UUID deriving(Eq)

instance ToField PersistUUID where
    toField (PersistUUID u) = toField $ toText u

instance FromField PersistUUID where
    fromField f = do bs <- fromField f
                     maybe (returnError ConversionFailed f "Could not parse UUID") (pure . PersistUUID) (fromText bs)

newtype PersistFieldValue = PersistFieldValue FieldValue

instance ToField PersistFieldValue where
    toField (PersistFieldValue NoField)           = toField Null
    toField (PersistFieldValue NullField)         = toField Null
    toField (PersistFieldValue (BoolField True))  = SQLInteger 1
    toField (PersistFieldValue (BoolField False)) = SQLInteger 0
    toField (PersistFieldValue (NumberField n))   = toField n
    toField (PersistFieldValue (TextField t))     = toField t

instance FromField PersistFieldValue where
    fromField f = case fieldData f of
        (SQLInteger i) -> pure $ PersistFieldValue $ NumberField $ fromIntegral i
        (SQLFloat n) -> pure $ PersistFieldValue $ NumberField n
        (SQLText t) -> pure $ PersistFieldValue $ TextField t
        (SQLBlob _) -> returnError ConversionFailed f "Field values cannot be blobs"
        SQLNull -> pure $ PersistFieldValue NullField

data ValueTransform = IdentityTransform | BoolTransform | NoFieldTransform

instance ToField ValueTransform where
    toField IdentityTransform = SQLNull
    toField BoolTransform     = SQLInteger 1
    toField NoFieldTransform  = SQLInteger 2

instance FromField ValueTransform where
    fromField f = case fieldData f of
        SQLNull        -> pure IdentityTransform
        (SQLInteger 1) -> pure BoolTransform
        (SQLInteger 2) -> pure NoFieldTransform
        _              -> returnError ConversionFailed f "Unknown transform"

data QueryRow = QueryRow PersistUUID T.Text Bool UTCTime UTCTime T.Text ValueTransform PersistFieldValue

instance FromRow QueryRow where
    fromRow = QueryRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

queryRowId :: QueryRow -> PersistUUID
queryRowId (QueryRow _id _ _ _ _ _ _ _) = _id

createDatabaseActions :: String -> IO (DatabaseActions Connection)
createDatabaseActions f = do
    pool <- createPool (open f) close 1 10 10
    pure $ DatabaseActions (withPooledTransaction pool)
        insertContent'
        insertFields'
        insertProperties'
        updateLastModified'
        getContent'

withPooledTransaction :: Pool Connection -> (Connection -> IO a) -> IO a
withPooledTransaction p a = withResource p (\conn -> Database.SQLite.Simple.withTransaction conn (a conn))


createSchema :: Connection -> IO ()
createSchema conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS content (id TEXT PRIMARY KEY,\
                                                     \ last_modified_time TEXT NOT NULL,\
                                                     \ created_time TEXT NOT NULL) WITHOUT ROWID"

    execute_ conn "CREATE TABLE IF NOT EXISTS field (content_id TEXT NOT NULL,\
                                                   \ version_date TEXT NOT NULL,\
                                                   \ key TEXT NOT NULL,\
                                                   \ transform INTEGER,\
                                                   \ value BLOB,\
                                                   \ PRIMARY KEY (content_id, version_date, key),\
                                                   \ FOREIGN KEY (content_id) REFERENCES content(id)) WITHOUT ROWID"

    execute_ conn "CREATE TABLE IF NOT EXISTS properties (content_id TEXT NOT NULL,\
                                                        \ version_date TEXT NOT NULL,\
                                                        \ active INTEGER NOT NULL,\
                                                        \ type TEXT NOT NULL,\
                                                        \ PRIMARY KEY (content_id, version_date),\
                                                        \ FOREIGN KEY (content_id) REFERENCES content(id)) WITHOUT ROWID"

    execute_ conn "CREATE INDEX IF NOT EXISTS idx_content_type ON properties(type)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_active ON properties(active)"


insertContent' :: Connection -> (UUID, UTCTime) -> IO ()
insertContent' c (_id, time) = executeNamed c sql [":id" := PersistUUID _id, ":time" := time]
    where sql = "INSERT INTO content VALUES (:id, :time, :time)"

updateLastModified' :: Connection -> (UUID, UTCTime) -> IO ()
updateLastModified' c (_id, time) = executeNamed c sql [":id" := PersistUUID _id, ":time" := time]
    where sql = "UPDATE content SET last_modified_time = :time WHERE id = :id"

insertProperties' :: Connection -> (UUID, UTCTime, T.Text, Bool) -> IO ()
insertProperties' c (_id, time, _type, _active) = executeNamed c sql [":id" := PersistUUID _id, ":time" := time, ":type" := _type, ":active" := _active]
    where sql = "INSERT INTO properties VALUES (:id, :time, :active, :type)"

insertFields' :: Connection -> (UUID, UTCTime, [(T.Text, FieldValue)]) -> IO ()
insertFields' c (cId, time, kvps) = executeMany c sql parameters
    where sql = "INSERT INTO field VALUES (?,?,?,?,?)"
          cId' = PersistUUID cId
          parameters = map (\(k, v) -> (cId', time, k, getTransform v, PersistFieldValue v)) kvps

getContent' :: Connection -> ContentQuery -> IO [Content]
getContent' c (ContentQuery _id _type act cont time limit) = groupRows <$> queryNamed c sql parameters  where
    parameters = [":contentId" := PersistUUID <$> _id,
                  ":type" := _type,
                  ":active" := act,
                  ":continue" := PersistUUID <$> cont,
                  ":maxDate" := time,
                  ":limit" := fromMaybe (-1) limit]
    sql = "\
\SELECT id,type,active,last_modified_time,created_time,field.key,field.transform,field.value FROM (\
\    SELECT content.*,properties.type,properties.active FROM content\
\    INNER JOIN (\
\        SELECT content_id, MAX(version_date) AS max_properties_date\
\        FROM properties\
\        WHERE (:contentId IS NULL OR content_id = :contentId)\
\        AND   (:continue IS NULL or content_id > :continue)\
\        AND   (:type IS NULL OR type = :type)\
\        AND   (:active IS NULL or active = :active)\
\        AND   (:maxDate IS NULL OR version_date <= :maxDate)\
\        GROUP BY content_id\
\    ) AS inner ON properties.content_id = inner.content_id AND properties.version_date = inner.max_properties_date \
\    INNER JOIN properties ON content.id = properties.content_id\
\    ORDER BY id\
\    LIMIT :limit\
\) as content \
\INNER JOIN field on content.id = field.content_id \
\INNER JOIN (\
\    SELECT content_id, MAX(version_date) AS max_field_date, key\
\    FROM field\
\    WHERE :maxDate IS NULL OR version_date <= :maxDate\
\    GROUP BY content_id, key\
\) AS inner ON field.content_id = inner.content_id AND field.version_date = inner.max_field_date AND field.key = inner.key \
\ORDER BY content.id"


groupRows :: [QueryRow] -> [Content]
groupRows = map mapGroup . groupBy ((==) `on` queryRowId) where
    mapGroup rs = let (QueryRow (PersistUUID _id) cType act modified created _ _ _) = head rs
                  in  Content _id cType act modified created (getFields rs)
    getFields = fromMaybe Data.Aeson.Null . fromKeyValuePairs . map getField
    getField (QueryRow _ _ _ _ _ k t (PersistFieldValue v)) = (k, transformFieldValue t v)

getTransform :: FieldValue -> ValueTransform
getTransform (BoolField _) = BoolTransform
getTransform NoField       = NoFieldTransform
getTransform _             = IdentityTransform

transformFieldValue :: ValueTransform -> FieldValue -> FieldValue
transformFieldValue IdentityTransform v           = v
transformFieldValue BoolTransform (NumberField d) = BoolField (d /= 0)
transformFieldValue _ _                           = NoField
