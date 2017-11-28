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

data ValueTransform = IdentityTransform | BoolTransform

instance ToField ValueTransform where
    toField IdentityTransform = SQLNull
    toField BoolTransform     = SQLInteger 1

instance FromField ValueTransform where
    fromField f = case fieldData f of
        SQLNull        -> pure IdentityTransform
        (SQLInteger 1) -> pure BoolTransform
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
        updateContent'
        insertFields'
        getContent'

withPooledTransaction :: Pool Connection -> (Connection -> IO a) -> IO a
withPooledTransaction p a = withResource p (\conn -> Database.SQLite.Simple.withTransaction conn (a conn))


createSchema :: Connection -> IO ()
createSchema conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS content (id TEXT PRIMARY KEY,\
                                                     \ type TEXT NOT NULL,\
                                                     \ active INTEGER NOT NULL,\
                                                     \ last_modified_time TEXT NOT NULL,\
                                                     \ created_time TEXT NOT NULL) WITHOUT ROWID"

    execute_ conn "CREATE INDEX IF NOT EXISTS idx_content_type ON content(type)"

    execute_ conn "CREATE TABLE IF NOT EXISTS field (content_id TEXT NOT NULL,\
                                                   \ version_date TEXT NOT NULL,\
                                                   \ key TEXT NOT NULL,\
                                                   \ transform INTEGER,\
                                                   \ value BLOB,\
                                                   \ PRIMARY KEY (content_id, version_date, key)) WITHOUT ROWID"


insertContent' :: Connection -> (UUID, T.Text, UTCTime) -> IO ()
insertContent' c (_id, _type, time) = executeNamed c sql [":id" := PersistUUID _id, ":type" := _type, ":time" := time]
    where sql = "INSERT INTO content VALUES (:id, :type, 1, :time, :time)"

updateContent' :: Connection -> (UUID, T.Text, UTCTime) -> IO ()
updateContent' c (_id, _type, time) = executeNamed c sql [":id" := PersistUUID _id, ":type" := _type, ":time" := time]
    where sql = "UPDATE content SET type = :type, last_modified = :time WHERE id = :id"

insertFields' :: Connection -> (UUID, UTCTime, [(T.Text, FieldValue)]) -> IO ()
insertFields' c (cId, time, kvps) = executeMany c sql parameters
    where sql = "INSERT INTO field VALUES (?,?,?,?,?)"
          cId' = PersistUUID cId
          parameters = map (\(k, v) -> (cId', time, k, getTransform v, PersistFieldValue v)) kvps

getContent' :: Connection -> ContentQuery -> IO [Content]
getContent' c (ContentQuery _id _type cont time limit act) = groupRows <$> queryNamed c sql parameters  where
    parameters = [":contentId" := PersistUUID <$> _id,
                  ":type" := _type,
                  ":continue" := PersistUUID <$> cont,
                  ":maxDate" := time,
                  ":limit" := fromMaybe (-1) limit,
                  ":active" := if act then SQLInteger 1 else SQLInteger 0]
    sql = "\
\SELECT id,type,active,last_modified_time,created_time,field.key,field.transform,field.value FROM (\
\    SELECT * FROM content\
\    WHERE (:contentId IS NULL OR id = :contentId)\
\    AND   (:type IS NULL OR type = :type)\
\    AND   (:continue IS NULL or id > :continue)\
\    AND   (:maxDate IS NULL OR created_time <= :maxDate)\
\    AND   (active = :active)\
\    ORDER BY id\
\    LIMIT :limit\
\) as content \
\INNER JOIN field on content.id = field.content_id \
\INNER JOIN (\
\    SELECT content_id, MAX(version_date) AS max_date, key\
\    FROM field\
\    WHERE :maxDate IS NULL OR version_date <= :maxDate\
\    GROUP BY content_id, key\
\) AS inner ON field.content_id = inner.content_id AND field.version_date = inner.max_date AND field.key = inner.key \
\ORDER BY content.id"


groupRows :: [QueryRow] -> [Content]
groupRows = map mapGroup . groupBy ((==) `on` queryRowId) where
    mapGroup rs = let (QueryRow (PersistUUID _id) cType a modified created _ _ _) = head rs
                  in  Content _id cType a modified created (getFields rs)
    getFields = fromMaybe Data.Aeson.Null . fromKeyValuePairs . map getField
    getField (QueryRow _ _ _ _ _ k t (PersistFieldValue v)) = (k, transformFieldValue t v)

getTransform :: FieldValue -> ValueTransform
getTransform (BoolField _) = BoolTransform
getTransform _             = IdentityTransform

transformFieldValue :: ValueTransform -> FieldValue -> FieldValue
transformFieldValue IdentityTransform v           = v
transformFieldValue BoolTransform (NumberField d) = BoolField (d /= 0)
transformFieldValue _ _                           = NullField
