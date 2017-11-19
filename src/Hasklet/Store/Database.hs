{-# LANGUAGE OverloadedStrings #-}

module Hasklet.Store.Database(
    createDatabaseActions,
    createSchema) where

import           Hasklet.Store.Types              hiding (withTransaction)

import           Data.Pool
import qualified Data.Text                        as T
import           Data.Time.Clock
import           Data.UUID
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Types

newtype PersistUUID = PersistUUID UUID

instance ToField PersistUUID where
    toField (PersistUUID u) = toField $ toByteString u

instance FromField PersistUUID where
    fromField f = do bs <- fromField f
                     maybe (returnError ConversionFailed f "Could not parse UUID") (pure . PersistUUID) (fromByteString bs)

newtype PersistFieldValue = PersistFieldValue FieldValue

instance ToField PersistFieldValue where
    toField (PersistFieldValue NullField)       = toField Null
    toField (PersistFieldValue (BoolField b))   = toField b
    toField (PersistFieldValue (DoubleField d)) = toField d
    toField (PersistFieldValue (TextField t))   = toField t

createDatabaseActions :: String -> IO (DatabaseActions Connection)
createDatabaseActions f = do
    pool <- createPool (open f) close 1 10 10
    pure $ DatabaseActions (withPooledTransaction pool)
        insertContent'
        insertFields'

withPooledTransaction :: Pool Connection -> (Connection -> IO a) -> IO a
withPooledTransaction p a = withResource p (\conn -> withTransaction conn (a conn))

createSchema :: Connection -> IO ()
createSchema conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS content (id BLOB PRIMARY KEY,\
                                                     \ text TEXT,\
                                                     \ active INTEGER,\
                                                     \ last_modified_time TEXT,\
                                                     \ created_time TEXT)"

    execute_ conn "CREATE TABLE IF NOT EXISTS field (content_id BLOB,\
                                                   \ version_date TEXT,\
                                                   \ type INTEGER,\
                                                   \ key TEXT,\
                                                   \ value BLOB,\
                                                   \ PRIMARY KEY (content_id, version_date, key))"

insertContent' :: Connection -> (UUID, T.Text, UTCTime) -> IO ()
insertContent' c (_id, _type, time) = executeNamed c sql ["id" := PersistUUID _id, "type" := _type, "time" := time]
    where sql = "INSERT INTO content VALUES (:id, :type, 1, :time, :time)"

insertFields' :: Connection -> (UUID, UTCTime, [(T.Text, FieldValue)]) -> IO ()
insertFields' c (cId, time, kvps) = executeMany c sql parameters
    where sql = "INSERT INTO field VALUES (?,?,?,?,?)"
          cId' = PersistUUID cId
          parameters = map (\(k, v) -> (cId', time, fieldType v, k, PersistFieldValue v)) kvps

fieldType :: FieldValue -> Int
fieldType NullField       = 0
fieldType (BoolField _)   = 1
fieldType (DoubleField _) = 2
fieldType (TextField _)   = 3

