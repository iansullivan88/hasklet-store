module Main where

import           Hasklet.Store.Database
import           Hasklet.Store.Types
import           Hasklet.Store.Web

import           Data.Monoid
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           System.Directory
import           System.FilePath

data Options = Options {
    port         :: Int,
    databasePath :: String
}

main :: IO ()
main = do
    curDir <- getCurrentDirectory
    let defaultPath = curDir </> "content-store.db"
        defaultPort = 80
    opt <- execParser (info (optionParser defaultPort defaultPath) mempty)
    let path = databasePath opt
        p = port opt
    putStrLn "Starting hasklet-store"
    putStrLn $ "port: " ++ show p
    putStrLn $ "database: " ++ show path
    da <- createDatabaseActions path 20
    withTransaction da createSchema
    run p (application da)

optionParser :: Int -> String -> Parser Options
optionParser dPort dPath = Options <$> portParser <*> pathParser where
    portParser = option auto $ long "port"
                     <> short 'p'
                     <> metavar "PORT"
                     <> help "port to use"
                     <> value dPort
    pathParser = strOption $ long "database"
                     <> short 'd'
                     <> metavar "DATABASE_PATH"
                     <> help "path to the sqlite database"
                     <> value dPath
