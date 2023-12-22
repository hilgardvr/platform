{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty (scotty, get, param, html, request, params, post)
import Text.Mustache (automaticCompile, object, ToMustache (toMustache), substitute)
import Text.Mustache.Types ((~>), TemplateCache)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find)
import Flow (getQuestionFlow, Question, getQuestionFor)
import GHC.Conc (pseq)
import Network.Wai (Request(requestBody))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Mustache.Compile (cacheFromList)
--import qualified Text.Mustache.Compile as index
--import Migrations (openDatabase, runMigrations)

port :: Int
port = 3000

searchSpace :: [FilePath]
searchSpace = ["./templates"]

indexTemplate :: String
indexTemplate = "index.mustache"

questionTemplate :: String
questionTemplate = "question.mustache"

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


--compileTemplates :: IO (Either ParseE TemplateCache)
--compileTemplates = do
--    compiledIndexTemplate <- automaticCompile searchSpace indexTemplate
--    compiledQuestionTemplate <- automaticCompile searchSpace questionTemplate
--    return cacheFromList [compiledIndexTemplate, compiledQuestionTemplate]
    

main :: IO ()
main = do
    questionFlow <- getQuestionFlow 
    compiledIndexTemplate <- automaticCompile searchSpace indexTemplate
    compiledQuestionTemplate <- automaticCompile searchSpace questionTemplate
    case compiledIndexTemplate of 
        Left err -> print err
        Right tmpl ->
            scotty port $ do
                get "/:org" $ do
                    p <- param "org"
                    let o = Organisation p
                        q = getQuestionFor Nothing questionFlow
                        t = case q of 
                            Just q' -> 
                                let oq = OrgQuestion o q'
                                in substitute tmpl oq
                            Nothing -> trace "No question found" substitute tmpl o
                    html $ trace (show t) TL.fromStrict t
                post "/:org/answer/:aid" $ do
                    ps <- params
                    r <- request
                    b <- liftIO $ requestBody r
                    let o = trace ("body:" ++ show b) searchParam "org" ps
                        org = Organisation o
                        i = searchParam "aid" ps
                        nextQ = getQuestionFor (Just $ TL.unpack i) questionFlow
                        t = case nextQ of
                            Just q -> 
                                let oq = OrgQuestion org q
                                in substitute tmpl oq
                            Nothing -> trace "No question found" substitute tmpl org
                    html $ trace (show t) TL.fromStrict t

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
