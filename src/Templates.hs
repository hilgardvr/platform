{-# LANGUAGE OverloadedStrings #-}

module Templates
( indexTemplate
, questionTemplate
, searchSpace
, Product
, OrgQuestion
, getOrg
, getOrgQuestion
, buildTemplate
) where
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Flow (Question, getQuestionForAnswer)
import Text.Mustache (ToMustache (toMustache), object, automaticCompile, compileTemplateWithCache, substitute)
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

getOrg :: TL.Text -> Product
getOrg = Product

instance ToMustache Product where
    toMustache (Product { name = n }) = 
        object ["name" ~> n]

data OrgQuestion = OrgQuestion 
    { prod :: Product
    , question :: Maybe Question
    } deriving (Show)

getOrgQuestion :: Product -> Maybe Question -> OrgQuestion
getOrgQuestion = OrgQuestion

instance ToMustache OrgQuestion where
    toMustache ( OrgQuestion { prod = o , question = q }) = 
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

buildTemplate :: Maybe String -> Product -> [Question] -> IO T.Text
buildTemplate aid o qf = 
    let 
        q = getQuestionForAnswer aid qf
        oq = OrgQuestion o q
    in do
        tc <- compiledTemplates
        tmpl <- compileTemplateWithCache searchSpace tc indexTemplate
        case tmpl of
            Left err -> error $ show err
            Right t -> return $ substitute t oq
