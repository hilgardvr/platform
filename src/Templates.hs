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
import Flow (Question, getQuestionForAnswer)
import Text.Mustache (ToMustache (toMustache), object, automaticCompile, compileTemplateWithCache, substitute, Template)
import Text.Mustache.Types ((~>), TemplateCache)
import Text.Mustache.Compile (cacheFromList)

searchSpace :: [FilePath]
searchSpace = ["./templates"]

indexTemplate :: FilePath
indexTemplate = "index.mustache"

questionTemplate :: FilePath
questionTemplate = "question.mustache"

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
    } deriving (Show)

getOrgQuestion :: Product -> Maybe Question -> ProductQuestion
getOrgQuestion = ProductQuestion

instance ToMustache ProductQuestion where
    toMustache ( ProductQuestion { prod = o , question = q }) = 
        object 
            [ "prod" ~> o
            , "question" ~> q
            ]

compiledTemplates :: IO TemplateCache
compiledTemplates = do
    compiledIndexTemplate <- automaticCompile searchSpace indexTemplate
    compiledQuestionTemplate <- automaticCompile searchSpace questionTemplate
    compiledHomeTemplate <- automaticCompile searchSpace homeTemplate
    let tmpls = sequence [compiledIndexTemplate, compiledQuestionTemplate, compiledHomeTemplate]
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
    

buildTemplate :: Maybe String -> Product -> [Question] -> IO T.Text
buildTemplate aid prod qf =
    let
        q = getQuestionForAnswer aid qf
        pq = ProductQuestion prod q
    in do
        case aid of 
            Nothing -> do
                t' <- templateOrError indexTemplate
                return $ substitute t' pq
            Just aid' -> do
                t' <- templateOrError questionTemplate
                return $ substitute t' pq
