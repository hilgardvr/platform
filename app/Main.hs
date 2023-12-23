{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty (scotty, get, param, html, request, params, post)
import Text.Mustache (automaticCompile, object, ToMustache (toMustache), substitute, Template)
import Text.Mustache.Types ((~>), TemplateCache)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find, Foldable (toList))
import Flow (getQuestionFlow, Question, getQuestionFor)
import GHC.Conc (pseq)
import Network.Wai (Request(requestBody))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Mustache.Compile (cacheFromList)

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

--lookupOrError :: String -> TemplateCache -> Template
--lookupOrError s tc = 
--    case lookup s (toList tc) of
--        Nothing -> error $ "No tempate key " ++ s
--        Just t -> t
--

compileTemplates :: IO TemplateCache
compileTemplates = do
    compiledIndexTemplate <- automaticCompile searchSpace indexTemplate
    compiledQuestionTemplate <- automaticCompile searchSpace questionTemplate
    let tmpls = sequence [compiledIndexTemplate, compiledQuestionTemplate]
    case tmpls of
        Left err -> error $ show err
        Right ts -> return $ cacheFromList ts

main :: IO ()
main = do
    questionFlow <- getQuestionFlow 
    compiledIndexTemplate <- automaticCompile searchSpace indexTemplate
    let indexTemplate = case compiledIndexTemplate of
            Left err -> error $ show err
            Right t -> t
    compiledQuestionTemplate <- automaticCompile searchSpace questionTemplate
    let questionTemplate = case compiledIndexTemplate of
            Left err -> error $ show err
            Right t -> t
    --ts <- compileTemplates
    --putStrLn $ show ts
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
                            Nothing -> trace "No question found" $ substitute indexTemplate o
                    html $ trace (show t) TL.fromStrict t
                post "/:org/answer/:aid" $ do
                    ps <- params
                    r <- request
                    b <- liftIO $ requestBody r
                    let o = searchParam "org" ps
                        org = Organisation o
                        i = searchParam "aid" ps
                        nextQ = trace ("body:" ++ show b ++ "\n") getQuestionFor (Just $ TL.unpack i) questionFlow
                        t = case nextQ of
                            Just q -> 
                                let oq = OrgQuestion org q
                                in case compiledQuestionTemplate of
                                    Left err -> error ("Template Error: " ++ show err)
                                    Right t' -> trace (show q) substitute t' oq
                            Nothing -> trace "No question found" $ substitute questionTemplate o
                    html $ trace (show t) TL.fromStrict t
