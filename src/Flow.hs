{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Flow 
    ( Question (answer_type)
    , Answer
    , validate
    , AnswerType (SingleSelect, FreeText)
    , AnswerMapping
    , getQuestionFlow
    , getQuestionForAnswer
    , getAnswerById
    ) where

import Text.Mustache (ToMustache (toMustache), object, (~>))
import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import Text.Read (readMaybe)

flowFile :: FilePath
flowFile = "./migrations/1.flow.json"

class Validatable a where
    validate :: a -> String -> Bool

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
instance Validatable Answer where
    validate ans val = 
        case answer_mapping ans of
            Age -> 
                let val' = readMaybe val :: Maybe Int
                in case val' of
                    Nothing -> False
                    Just a -> a >= 18 && a <= 65
            Plan -> True
            Id -> True
            _ -> True

instance ToMustache Answer where
    toMustache a = object 
        [ "aid" ~> aid a
        , "answer_description" ~> answer_description a
        ]

data AnswerType = FreeText | SingleSelect deriving (Show, Generic)

instance FromJSON AnswerType
instance ToJSON AnswerType

data AnswerMapping = 
    Age 
    | Gender 
    | Plan 
    | Id deriving (Show, Generic)

instance FromJSON AnswerMapping
instance ToJSON AnswerMapping

getQuestionFlow :: IO [Question]
getQuestionFlow = do
    bjson <- B.readFile flowFile
    case decode bjson of
        Nothing -> error "Could not decode flow json"
        Just d -> return d

getAnswerById :: String -> [Question] -> Maybe Answer
getAnswerById ansId qs = 
    let
        ans' = concatMap answers qs
    in find (\a -> aid a == ansId)  ans'

getQuestionForAnswer :: Maybe String -> [Question] -> Maybe Question
getQuestionForAnswer Nothing qs = find (\e -> qid e == "1") qs
getQuestionForAnswer (Just ans) qs = 
    let
        ans' = concatMap answers qs
    in
    case find (\a -> aid a == ans)  ans' of 
        Nothing -> Nothing
        Just a -> 
            case next_question a of 
                Nothing -> Nothing 
                Just q' -> find (\e -> qid e == q') qs
