{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty (scotty, get, param, html, request, params)
import Text.Mustache (automaticCompile, object, ToMustache (toMustache), substitute)
import Text.Mustache.Types ((~>))
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find)
import Flow (getQuestionFlow, Question, getQuestionFor)
--import qualified Text.Mustache.Compile as index
--import Migrations (openDatabase, runMigrations)

port :: Int
port = 3000

searchSpace :: [FilePath]
searchSpace = ["./templates"]

templateName :: String
templateName = "index.mustache"

dbName :: String
dbName = "platform"

newtype Organisation = Organisation { name :: TL.Text } deriving (Show)

instance ToMustache Organisation where
    toMustache (Organisation { name = n }) = 
        object ["name" ~> n]

data OrgQuestion = OrgQuestion 
    { org :: Organisation
    , question :: Question
    } deriving (Show)

instance ToMustache OrgQuestion where
    toMustache ( OrgQuestion { org = o , question = q }) = 
        object 
            [ "org" ~> o
            , "question" ~> q
            ]

searchParam :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
searchParam p ps = case snd <$> find (\e -> fst e == p) ps of
    Nothing -> error $ "No param found for: " ++ show p
    Just p' -> p'


main :: IO ()
main = do
    questionFlow <- getQuestionFlow
    compiled <- automaticCompile searchSpace templateName
    case compiled of 
        Left err -> print err
        Right tmpl ->
            scotty port $ do
                get "/:org" $ do
                    p <- param "org"
                    let o = trace (show p) Organisation p
                        q = getQuestionFor Nothing questionFlow
                        t = case q of 
                            Just q' -> 
                                let oq = OrgQuestion o q'
                                in trace (show oq) substitute tmpl oq
                            Nothing -> trace "No question found" substitute tmpl o
                    html $ trace ("templ: " ++ show t) $ TL.fromStrict t
                --get "/:org/:qid" $ do
                --    r <- request
                --    --b <- liftIO $ requestBody r
                --    ps <- params
                --    let o = searchParam "org" ps
                --        i = searchParam "qid" ps
                --        org = Organisation o
                --        q = Question {
                --                , qid = i
                --            }
                --        t = substitute tmpl q
                --    html $ trace ("templ: " ++ show t) $ TL.fromStrict t
