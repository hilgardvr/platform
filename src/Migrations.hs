module Migrations 
( openDatabase
) where 

import qualified Data.Text as T
import System.Directory (getDirectoryContents)
import Data.List (sortBy, elemIndex)
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Database.SQLite.Simple (Query (Query), Connection, execute_, open)

migrationDir :: FilePath
migrationDir = "./migrations/"


    
getMigrationNumber :: String -> Maybe Int 
getMigrationNumber mig = 
    case elemIndex '.' mig of
        Nothing -> Nothing 
        Just i' -> readMaybe (take i' mig) :: Maybe Int

createQuery :: String -> Query
createQuery = Query . T.pack

doMigrations :: Connection -> [String] -> IO ()
doMigrations conn ms = do
    print ("migratios: " ++ show ms)
    mapM_ (\e -> trace ("Running migration: " ++ show e ++ "\n") $ (execute_ conn . createQuery) e)  ms
    

getMigrations :: IO [String]
getMigrations = do
    migrations <- getDirectoryContents migrationDir
    let valid = filter (isJust . getMigrationNumber) migrations
    let sorted = sortBy (\a b -> compare (getMigrationNumber a) (getMigrationNumber b)) valid
    trace ("Sorted: " ++ show sorted) $ mapM (\e -> readFile $ migrationDir ++ e) sorted

runMigrations :: Connection -> IO ()
runMigrations conn = do
    migrations <- getMigrations
    trace ("Migrations: " ++ show migrations) $ doMigrations conn migrations


openDatabase :: String -> IO Connection
openDatabase dbName = do
    con <- open dbName
    runMigrations con
    return con
