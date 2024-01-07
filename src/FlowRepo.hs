{-# LANGUAGE OverloadedStrings #-}

module FlowRepo
( insertFlowAnswer
, getFlow
) where
import Database.SQLite.Simple (Connection, Query (Query), FromRow (fromRow), ToRow (toRow), field, execute, query_)
import qualified Data.Text as T

createQuery :: String -> Query
createQuery = Query . T.pack

data Flow = Flow
    { id :: String
    , product :: String
    , qid :: String
    , question_description :: String
    , aid :: String
    , answer :: String
    , status :: String
    } deriving (Show)

instance FromRow Flow where
    fromRow = Flow <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Flow where
    toRow (Flow id' product' qid' question_description' aid' answer' status') = toRow (id', product', qid', question_description', aid', answer', status')

insertFlowAnswer :: Connection -> Flow -> IO ()
insertFlowAnswer conn flow = do
    execute conn (createQuery "insert into flow (id, product, qid, question_description, aid, answer, status) values (?,?,?,?,?,?,?)") flow

getFlow :: String -> String -> String -> String -> String -> String -> String -> Flow
getFlow = Flow


getAllFlow :: Connection -> IO [Flow]
getAllFlow conn =
    let query = createQuery "select * from flow"
    in do query_ conn query
    
