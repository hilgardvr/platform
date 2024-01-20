{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FlowService
( handleFlow
) where
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy.Internal as BSL
import Debug.Trace (trace)
import qualified Flow as F
import qualified FlowRepo as FR
import Database.SQLite.Simple (Connection)
import Flow (getQuestionFromAnswerId, Question)
import Templates (buildTemplate, getProduct)
import Data.Aeson (ToJSON (toJSON), object, (.=), FromJSON (parseJSON), (.:), withObject, decode, encode)
import GHC.Generics (Generic)
import FlowRepo (getFlowForSession)
import qualified Data.List as DL
import Data.UUID (UUID, fromString)
import System.Random (mkStdGen, Random (random))
import Network.HTTP.Client.Conduit (parseRequest, Request (method), RequestBody (RequestBodyBS))
import Network.HTTP.Simple (httpBS, setRequestBody, getResponseBody, setRequestBasicAuth)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Internal.Builder as BS
import qualified Data.ByteString as BS

data ProductPricingResponseDTO = ProductPricingResponseDTO 
    { plans' :: [PlanQuoteDTO]
    } deriving (Read, Show)

instance ToJSON ProductPricingResponseDTO where 
    toJSON (ProductPricingResponseDTO p) =
        object 
            [ "plans" .= p]

instance FromJSON ProductPricingResponseDTO where 
    parseJSON = withObject "ProductPricingResponseDTO" $ \v -> ProductPricingResponseDTO
        <$> v .: "plans"

data PlanQuoteDTO = PlanQuoteDTO 
    { correlationId :: String
    , status :: PlanQuoteStatus
    , premium :: Integer
    , benefits :: [PlanQuoteBenefitDTO]
    , coveredLives :: [PlanQuoteCoverLifeDTO]
    } deriving (Read, Show, Generic)

instance ToJSON PlanQuoteDTO
instance FromJSON PlanQuoteDTO

data PlanQuoteStatus = ACCEPTED deriving (Read, Show, Generic)
instance ToJSON PlanQuoteStatus
instance FromJSON PlanQuoteStatus

data PlanQuoteBenefitDTO = PlanQuoteBenefitDTO
    { name :: String
    , value :: String
    } deriving (Read, Show, Generic)

instance ToJSON PlanQuoteBenefitDTO
instance FromJSON PlanQuoteBenefitDTO

data PlanQuoteCoverLifeDTO = PlanQuoteCoverLifeDTO 
    { pqclCorrelationId :: String
    , pqclPremium :: Integer
    , pqclStatus :: ProductQuoteCoveredLifeStatus
    , pqclBenefits :: [PlanQuoteBenefitDTO]
    } deriving (Read, Show)

instance ToJSON PlanQuoteCoverLifeDTO where 
    toJSON (PlanQuoteCoverLifeDTO c p s b) =
        object 
            [ "correlationId" .= c
            , "premium" .= p 
            , "status" .= s
            , "benefits" .= b
            ]

instance FromJSON PlanQuoteCoverLifeDTO where 
    parseJSON = withObject "PlanQuoteCoverLifeDTO" $ \v -> PlanQuoteCoverLifeDTO
        <$> v .: "correlationId"
        <*> v .: "premium"
        <*> v .: "status"
        <*> v .: "benefits"

data ProductQuoteCoveredLifeStatus = ELIGIBLE deriving (Read, Show, Generic)
instance ToJSON ProductQuoteCoveredLifeStatus
instance FromJSON ProductQuoteCoveredLifeStatus
 
data CoveredLifeType =
    MAIN
    | SPOUSE
    | CHILD
    | PARENT
    | EXTENDED_FAMILY deriving (Read, Show, Generic)

instance ToJSON CoveredLifeType
instance FromJSON CoveredLifeType

data Gender =
    MALE |
    FEMALE deriving (Read, Show, Generic)

instance ToJSON Gender
instance FromJSON Gender

data ProductCoveredLifeDTO = ProductCoveredLifeDTO 
    { age :: Int
    , correlationId' :: String
    , coverAmount :: Maybe Integer
    , gender :: Gender
    , type' :: CoveredLifeType 
    } deriving (Read, Show, Generic)

instance ToJSON ProductCoveredLifeDTO where
    toJSON (ProductCoveredLifeDTO age correlationId coverAmount gender type') =
        object 
            [ "age" .= age
            , "correlationId" .= correlationId
            , "coverAmount" .= coverAmount
            , "gender" .= gender
            , "type" .= type'
            ]

instance FromJSON ProductCoveredLifeDTO where 
    parseJSON = withObject "ProductCoveredLifeDTO" $ \v -> ProductCoveredLifeDTO
        <$> v .: "age"
        <*> v .: "correlationId"
        <*> v .: "coverAmount"
        <*> v .: "gender"
        <*> v .: "type"

data ProductPlanName = 
    UNIVERSITY |
    HOME |
    CAR |
    FOOD deriving (Read, Show, Generic)
    --VANILLA_FUNERAL |
    --DIAMOND_PLAN 
instance ToJSON ProductPlanName
instance FromJSON ProductPlanName
    
data PlanDto = PlanDto 
    { planCorrelationId :: UUID
    , plan :: String --ProductPlanName
    } deriving (Read, Show, Generic)

instance ToJSON PlanDto
instance FromJSON PlanDto

data ProductPricingRequestDTO = ProductPricingRequestDTO 
    { product' :: String
    , plans :: [PlanDto]
    , coveredLives' :: [ProductCoveredLifeDTO]
    , commencementDate :: String
    , intermediaryId :: Maybe UUID
    , leadSourceId :: Maybe UUID
    } deriving (Read, Show, Generic)

instance ToJSON ProductPricingRequestDTO where
    toJSON (ProductPricingRequestDTO product' plans coveredLives' commencementDate intermediaryId leadSourceId) =
        object 
            [ "product" .= product'
            , "plans" .= plans
            , "coveredLives" .= coveredLives'
            , "commencementDate" .= commencementDate
            , "intermediaryId" .= intermediaryId
            , "leadSourceId" .= leadSourceId
            ]

makeQuestionAnswer :: [FR.Flow] -> [Question] -> [(Question, FR.Flow)]
makeQuestionAnswer [] _ = []
makeQuestionAnswer (h:t) qs = 
    let
        q = DL.find (\e -> FR.qid h == F.qid e) qs
    in
        case q of 
            Nothing -> error $ "Could not find question for answer. qid: " ++ FR.qid h  ++ " aid: " ++ FR.aid h
            Just q' -> (q', h) : makeQuestionAnswer t qs

dummyIntermediaryDigi = case fromString "c2334019-a414-460a-9e9e-a9df887827f3" of
    Nothing -> error "Error converting to UUID"
    Just i -> i

randUUID :: UUID
randUUID =
    let 
        seed = 123
        g0 = mkStdGen seed
        (u1, g1) = random g0
    in u1

buildCoveredLife :: [FR.Flow] -> ProductCoveredLifeDTO
buildCoveredLife flow =
    let
        age = trace "start buildCoveredLife" FR.answer . head $ filter (\e -> FR.answer_mapping e == "Age") flow
        corId = trace ("age" ++ show age) randUUID 
        coverAmount = trace (show corId) Just 50000
        gend = FR.answer . head $ filter (\e -> FR.answer_mapping e == "Gender") flow
        type' = FR.answer . head $ filter (\e -> FR.answer_mapping e == "Plan") flow
    in
        trace (show gend) ProductCoveredLifeDTO (read age :: Int) (show corId) coverAmount (MALE) MAIN


getPricingRequest :: Connection -> FR.SessionId -> IO ProductPricingRequestDTO
getPricingRequest conn sess = do
    sessionFlow <- getFlowForSession sess conn
    let plan = "car"--trace (show sessionFlow) FR.answer . head $ filter (\e -> FR.answer_mapping e == "Plan") sessionFlow
        planDto = trace ("plan" ++ show plan)  [PlanDto randUUID ( plan )]
        coveredLives = trace ("plandto: " ++ show planDto) buildCoveredLife sessionFlow
        commencementDate = trace (show coveredLives) FR.answer . head $ filter (\e -> FR.answer_mapping e == "CoverStart") sessionFlow
        intermediaryId = Nothing
        leadSourceId = Nothing
    return $ ProductPricingRequestDTO plan planDto [coveredLives] commencementDate intermediaryId leadSourceId

makePricingRequest :: ProductPricingRequestDTO -> IO ProductPricingResponseDTO
makePricingRequest pr = do
    initReq <- parseRequest "http://localhost:8080/api/v1/prices/product-plan/quote"
    let json = toJSON pr
        req = initReq { method = "POST" }
        bod = BS.packChars (show (encode json))
        reqBody = trace ("rqbody: " ++ show bod ++ "\n") RequestBodyBS bod
        reqWithBod = setRequestBody reqBody req
        reqWithAuth = setRequestBasicAuth "onesparkadmin" "supersecurepassword" reqWithBod
    res <- httpBS reqWithAuth
    let result = case decode (BSL.packChars $ show $ getResponseBody res) of
            Nothing -> error $ "Failed to decode response: " ++ show res
            Just v -> v
    trace (show result) return result


handleFlow :: FR.ProductName -> FR.AnswerId -> FR.Answer -> FR.SessionId -> [Question] -> Connection -> IO TS.Text
handleFlow prod aid userAnswer sess questionFlow conn = do
    let answer = trace ("userAnswer: " ++ show userAnswer) F.getAnswerById aid questionFlow
        questionAnswered = getQuestionFromAnswerId (Just aid) questionFlow
        valid = F.validate answer userAnswer
        f = FR.buildFlow Nothing prod (F.qid questionAnswered) (F.description questionAnswered) aid userAnswer (show $ F.answer_mapping answer) (if valid then "VALID" else "INVALID") sess
    _ <- FR.insertFlowAnswer conn f
    case F.answer_type questionAnswered of
        F.Finalise -> do
            pricingReq <- getPricingRequest conn sess
            pricingRes <- trace (show pricingReq) makePricingRequest pricingReq
            trace ("response: " ++ show pricingRes) buildTemplate (Just aid) (if valid then Nothing else Just "Validation failed") (getProduct $ TL.pack prod) questionFlow
        _ -> buildTemplate (Just aid) (if valid then Nothing else Just "Validation failed") (getProduct $ TL.pack prod) questionFlow

        
