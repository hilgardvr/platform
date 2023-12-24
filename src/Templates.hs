{-# LANGUAGE OverloadedStrings #-}

module Templates
( indexTemplate
, questionTemplate
, searchSpace
, Organisation
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

newtype Organisation = Organisation { name :: TL.Text } deriving (Show)

getOrg :: TL.Text -> Organisation
getOrg = Organisation

instance ToMustache Organisation where
    toMustache (Organisation { name = n }) = 
        object ["name" ~> n]

data OrgQuestion = OrgQuestion 
    { org :: Organisation
    , question :: Maybe Question
    } deriving (Show)

getOrgQuestion :: Organisation -> Maybe Question -> OrgQuestion
getOrgQuestion = OrgQuestion

instance ToMustache OrgQuestion where
    toMustache ( OrgQuestion { org = o , question = q }) = 
        object 
            [ "org" ~> o
            , "question" ~> q
            ]

compiledTemplates :: IO TemplateCache
compiledTemplates = do
    compiledIndexTemplate <- automaticCompile searchSpace indexTemplate
    compiledQuestionTemplate <- automaticCompile searchSpace questionTemplate
    let tmpls = sequence [compiledIndexTemplate, compiledQuestionTemplate]
    case tmpls of
        Left err -> error $ show err
        Right ts -> return $ cacheFromList ts

buildTemplate :: Maybe String -> Organisation -> [Question] -> IO T.Text
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
