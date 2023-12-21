{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Flow 
    ( Question
    , Answer
    , AnswerType
    , AnswerMapping
    , getQuestionFlow
    , getQuestionFor
    ) where

import Text.Mustache (ToMustache (toMustache), object, (~>))
import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.List (find)

flowFile :: FilePath
flowFile = "./migrations/1.flow.json"


data Question = Question 
    { qid :: String 
    , description :: String
    , answer_type :: AnswerType
    , answers :: [Answer]
    } deriving (Show, Generic)

instance ToMustache Question where
    toMustache q = object 
        [ "qid" ~> qid q
        , "description" ~> description q 
        , "answers" ~> answers q
        ]

instance FromJSON Question
instance ToJSON Question

data Answer = Answer
    { aid :: String
    , answer_description :: String
    , answer_mapping :: AnswerMapping
    , next_question :: Maybe String
    } deriving (Show, Generic)

instance FromJSON Answer
instance ToJSON Answer

instance ToMustache Answer where
    toMustache a = object 
        [ "aid" ~> aid a
        , "answer_description" ~> answer_description a
        ]

data AnswerType = FreeText | SingleSelect deriving (Show, Generic)

instance FromJSON AnswerType
instance ToJSON AnswerType

data AnswerMapping = Age | Gender deriving (Show, Generic)

instance FromJSON AnswerMapping
instance ToJSON AnswerMapping

getQuestionFlow :: IO [Question]
getQuestionFlow = do
    bjson <- B.readFile flowFile
    case decode bjson of
        Nothing -> error "Could not decode flow json"
        Just d -> return d

getQuestionFor :: Maybe String -> [Question] -> Maybe Question
getQuestionFor Nothing qs = find (\e -> qid e == "1") qs
getQuestionFor (Just q) qs = find (\e -> qid e == q) qs
