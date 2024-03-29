{-# LANGUAGE OverloadedStrings #-}

module FlowRepo
( insertFlowAnswer
, buildFlow
, getAllFlow
, FlowId
, ProductName
, QuestionId
, QuestionDescription
, AnswerId
, Answer
, Status
, SessionId
, getFlowForSession
, Flow
, qid
, aid
, answer_mapping
, answer
) where
import Database.SQLite.Simple (Connection, Query (Query), FromRow (fromRow), ToRow (toRow), field, execute, query_, queryNamed, NamedParam((:=)))
import qualified Data.Text as T

type FlowId = Integer
type ProductName = String
type QuestionId = String
type QuestionDescription = String
type AnswerId = String
type Answer = String
type AnswerMapping = String
type Status = String
type SessionId = String

createQuery :: String -> Query
createQuery = Query . T.pack

data Flow = Flow
    { id :: Maybe FlowId
    , product :: ProductName
    , qid :: QuestionId
    , question_description :: QuestionDescription
    , aid :: AnswerId
    , answer :: Answer
    , answer_mapping :: AnswerMapping
    , status :: Status
    , sess :: SessionId
    } deriving (Show)

instance FromRow Flow where
    fromRow = Flow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Flow where
    toRow (Flow id' product' qid' question_description' aid' answer' answer_mapping' status' ses') = toRow (id', product', qid', question_description', aid', answer', answer_mapping', status', ses')

insertFlowAnswer :: Connection -> Flow -> IO ()
insertFlowAnswer conn flow = do
    execute conn (createQuery "insert into flow (id, product, qid, question_description, aid, answer, answer_mapping, status, sess) values (?,?,?,?,?,?,?,?,?)") flow

buildFlow :: Maybe FlowId -> ProductName -> QuestionId -> QuestionDescription -> AnswerId -> Answer -> AnswerMapping -> Status -> SessionId -> Flow
buildFlow = Flow

getAllFlow :: Connection -> IO [Flow]
getAllFlow conn =
    let query = createQuery "select * from flow"
    in do query_ conn query
    
getFlowForSession :: SessionId -> Connection -> IO [Flow]
getFlowForSession sessId conn =
    let q = createQuery "select * from flow where sess = :sessId and status = 'VALID'"
    in do queryNamed conn q [":sessId" := sessId]
