module Main where

import           Hasklet.Store.Database
import           Hasklet.Store.Types
import           Hasklet.Store.Web

import           Network.Wai.Handler.Warp
import           System.Directory
import           System.FilePath

main :: IO ()
main = do
    let port = 8000
    curDir <- getCurrentDirectory
    let path = curDir </> "content-store.db"
    da <- createDatabaseActions path 20
    withTransaction da createSchema
    run port (application da)

