module Migrations (
    openDatabase
    , getAllQuestions
    , runMigrations
    , createQuery
) where 

import qualified Data.Text as T
import Database.SQLite.Simple (Query (Query), Connection, open, execute_, query, query_, field, FromRow (fromRow))
import System.Directory (getDirectoryContents)
import Data.List (sortBy, elemIndex)
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Control.Monad (forM_)

data Question = Question {
    qid :: String
    , description :: String
} deriving (Show)

instance FromRow Question where
    fromRow = Question <$> field <*> field
    

migrationDir :: String
migrationDir = "./migrations/"

getAllQuestionsQuery :: Query
getAllQuestionsQuery = Query (T.pack "select qid, description from questions;") 

getAllQuestions :: Connection -> IO [Question]
getAllQuestions conn = query_ conn getAllQuestionsQuery :: IO [Question]
    
getMigrationNumber :: String -> Maybe Int 
getMigrationNumber mig = 
    case elemIndex '.' mig of
        Nothing -> Nothing --error "Invalid migration number"
        Just i' -> readMaybe (take i' mig) :: Maybe Int

createQuery :: String -> Query
createQuery = Query . T.pack

doMigrations :: Connection -> [String] -> IO ()
doMigrations conn ms = do
    print ("migratios: " ++ show ms)
    mapM_ (\e -> trace ("Running migration: " ++ show e ++ "\n") $ (execute_ conn . Query . T.pack) e) (drop 2 ms)
    

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
    open dbName
