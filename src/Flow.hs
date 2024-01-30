{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Flow 
( Question (answer_type)
, Answer
, validate
, AnswerType (SingleSelect, FreeText, DatePicker, Finalise)
, AnswerMapping
, getQuestionFlow
, getNextQuestionForAnswer
, getAnswerById
, getQuestionFromAnswerId
, qid
, description
, answer_mapping
) where

import Text.Mustache (ToMustache (toMustache), object, (~>))
import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import Text.Read (readMaybe)
import Debug.Trace (trace)

flowFile :: FilePath
flowFile = "./product/protector.1.json"

class Validatable a where
    validate :: a -> String -> Maybe String

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
        let am = trace ("AnswerMapping: " ++ (show $ answer_mapping ans) ++ "\n") answer_mapping ans
        in case am of
            Age -> 
                let 
                    val' = trace ("AnswerMapping2: " ++ (show $ answer_mapping ans) ++ "\n") readMaybe val :: Maybe Int
                    msg = "Age needs to be between 18 and 65"
                in case val' of
                    Nothing -> trace ("read: " ++ show val') Just msg
                    Just a -> 
                        if a >= 18 && a <= 65
                        then Nothing
                        else Just msg
            Plan -> Nothing
            Id -> Nothing
            _ -> Nothing

instance ToMustache Answer where
    toMustache a = object 
        [ "aid" ~> aid a
        , "answer_description" ~> answer_description a
        ]

data AnswerType = 
    FreeText 
    | SingleSelect
    | DatePicker 
    | Finalise deriving (Show, Generic)

instance FromJSON AnswerType
instance ToJSON AnswerType

data AnswerMapping = 
    Age 
    | Gender 
    | Plan 
    | Id 
    | Title
    | FirstName
    | LastName
    | Email
    | MobileNumber
    | Address
    | CoverStart
    | PaymentMethod
    | Bank
    | BankAccountNumber
    | BankAccountType
    | AcceptsDebit
    | PersalNumber
    | SourceOfFunds
    | PersonalDetails 
    | IssuePolicy deriving (Show, Generic, Read)

instance FromJSON AnswerMapping
instance ToJSON AnswerMapping

getQuestionFlow :: IO [Question]
getQuestionFlow = do
    bjson <- B.readFile flowFile
    case decode bjson of
        Nothing -> error "Could not decode flow json"
        Just d -> return d

getAnswerById :: String -> [Question] -> Answer
getAnswerById ansId qs = 
    let ans' = concatMap answers qs
        found =  find (\a -> aid a == ansId)  ans'
    in case found of
        Nothing -> error $ "No answer found for answerId: " ++ ansId
        Just a -> a

getNextQuestionForAnswer :: Maybe String -> [Question] -> Maybe Question
getNextQuestionForAnswer Nothing qs = find (\e -> qid e == "1") qs
getNextQuestionForAnswer (Just ans) qs = 
    let
        ans' = concatMap answers qs
    in
    case find (\a -> aid a == ans)  ans' of 
        Nothing -> Nothing
        Just a -> 
            case next_question a of 
                Nothing -> Nothing 
                Just q' -> find (\e -> qid e == q') qs

getQuestionFromAnswerId :: Maybe String -> [Question] -> Question
getQuestionFromAnswerId Nothing qs = 
    let q = find (\e -> qid e == "1") qs
    in case q of 
        Nothing -> error "Could not find question id 1"
        Just q' -> q'
getQuestionFromAnswerId (Just ans) qs = 
    let q = find (\q' -> 
            let 
                q_ans' = answers q'
                maybeA = find (\a -> aid a == ans) q_ans'
            in
            case maybeA of
                Nothing -> False
                Just _ -> True
            ) qs
    in case q of
        Nothing -> error $ "No question found for answerId: " ++ ans
        Just q' -> q'
