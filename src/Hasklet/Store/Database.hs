module Hasklet.Store.Database(createSqlitePool,withPooledTransaction) where

import           Data.Pool
import           Data.Time.Clock
import           Database.SQLite.Simple


createSqlitePool :: String -> IO (Pool Connection)
createSqlitePool f = createPool (open f) close 1 10 10

withPooledTransaction :: Pool Connection -> (Connection -> IO a) -> IO a
withPooledTransaction p a = withResource p (\conn -> withTransaction conn (a conn))

