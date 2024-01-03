{-# LANGUAGE OverloadedStrings #-}

module Templates
( indexTemplate
, questionTemplate
, searchSpace
, Product
, ProductQuestion
, getProduct
, getOrgQuestion
, buildTemplate
) where
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Flow (Question (answer_type), getNextQuestionForAnswer, AnswerType (SingleSelect, FreeText), getQuestionFromAnswerId)
import Text.Mustache (ToMustache (toMustache), object, automaticCompile, compileTemplateWithCache, substitute, Template)
import Text.Mustache.Types ((~>), TemplateCache)
import Text.Mustache.Compile (cacheFromList)
import Debug.Trace (trace)

searchSpace :: [FilePath]
searchSpace = ["./templates"]

indexTemplate :: FilePath
indexTemplate = "index.mustache"

questionTemplate :: FilePath
questionTemplate = "question.mustache"

singleSelectQuestionTemplate :: FilePath
singleSelectQuestionTemplate = "single-select-question.mustache"

homeTemplate :: FilePath
homeTemplate = "home.mustache"

newtype Product = Product { name :: TL.Text } deriving (Show)

getProduct :: TL.Text -> Product
getProduct = Product

instance ToMustache Product where
    toMustache (Product { name = n }) = 
        object ["name" ~> n]

data ProductQuestion = ProductQuestion 
    { prod :: Product
    , question :: Maybe Question
    , err :: Maybe String
    } deriving (Show)

getOrgQuestion :: Product -> Maybe Question -> Maybe String -> ProductQuestion
getOrgQuestion = ProductQuestion

instance ToMustache ProductQuestion where
    toMustache ( ProductQuestion { prod = o , question = q, err = e }) = 
        object 
            [ "prod" ~> o
            , "question" ~> q
            , "err" ~> e
            ]

compiledTemplates :: IO TemplateCache
compiledTemplates = do
    compiledIndexTemplate <- automaticCompile searchSpace indexTemplate
    compiledQuestionTemplate <- automaticCompile searchSpace questionTemplate
    compiledSingleSelectTemplate <- automaticCompile searchSpace singleSelectQuestionTemplate
    compiledHomeTemplate <- automaticCompile searchSpace homeTemplate
    let tmpls = sequence [compiledIndexTemplate, compiledQuestionTemplate, compiledHomeTemplate, compiledSingleSelectTemplate]
    case tmpls of
        Left err -> error $ show err
        Right ts -> return $ cacheFromList ts

templateOrError :: String -> IO Template
templateOrError tmpl = do
    tc <- compiledTemplates
    tmpl' <- compileTemplateWithCache searchSpace tc tmpl
    case tmpl' of
        Left err -> error $ show err
        Right t' -> return t'
    

buildTemplate :: Maybe String -> Maybe String -> Product -> [Question] -> IO T.Text
buildTemplate aid err prod' qf =
    let
        (q, pq) = case err of
            Nothing -> 
                let
                    q = getNextQuestionForAnswer aid qf
                    pq = trace ("Next q: " ++ show q) ProductQuestion prod' q Nothing
                in
                    (q, pq)
            Just err' -> 
                let
                    q = getQuestionFromAnswerId aid qf
                    pq = trace ("Err: " ++ err') ProductQuestion prod' q (Just err')
                in
                    (q, pq)
    in
        case aid of 
            Nothing -> do
                t' <- templateOrError indexTemplate
                return $ substitute t' pq
            Just aid' ->
                case q of
                    Nothing -> do
                        t' <- templateOrError questionTemplate
                        return $ substitute t' pq
                    Just q' -> do
                        let answerType = answer_type q'
                        case answerType of
                            SingleSelect -> do
                                t' <- templateOrError singleSelectQuestionTemplate
                                return $ substitute t' pq
                            FreeText -> do
                                t' <- templateOrError questionTemplate
                                return $ substitute t' pq
                            _ -> error $ "No answer type implemented for " ++ show answerType
                
                            
                            

