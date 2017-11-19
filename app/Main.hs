module Main where

import           Hasklet.Store.Database
import           Hasklet.Store.Types
import           Hasklet.Store.Web

import           System.Directory
import           System.FilePath

main :: IO ()
main = do
    let port = 8000
    curDir <- getCurrentDirectory
    let path = curDir </> "content-store.db"
    da <- createDatabaseActions path
    withTransaction da createSchema
    startServer port da

