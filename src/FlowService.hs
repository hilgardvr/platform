{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FlowService
( handleFlow
) where
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import qualified Flow as F
import qualified FlowRepo as FR
import Database.SQLite.Simple (Connection)
import Flow (getQuestionFromAnswerId, Question)
import Templates (buildTemplate, getProduct)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import GHC.Generics (Generic)
import FlowRepo (getFlowForSession)
import qualified Data.List as DL
import Data.UUID (UUID)
import System.Random (mkStdGen, Random (random))

--data class ProductPricingResponseDTO(
--    val plans: List<PlanQuoteDTO>
--)
--data class PlanQuoteDTO(
--    val correlationId: String,
--    val status: PlanQuoteStatus,
--    val premium: BigInteger,
--    val benefits: List<PlanQuoteBenefitDTO>,
--    val coveredLives: List<PlanQuoteCoverLifeDTO>
--)
--enum class PlanQuoteStatus(@JsonValue val alias: String) {
--    ACCEPTED("accepted")
--}
--data class PlanQuoteBenefitDTO(
--    val name: String,
--    val value: String
--) {
--data class PlanQuoteBenefitDTO(
--    val name: String,
--    val value: String
--) {
--enum class ProductQuoteCoveredLifeStatus(@JsonValue val alias: String) {
--    ELIGIBLE("eligible")
--}
--data class PlanQuoteBenefitDTO(
--    val name: String,
--    val value: String
--) {

--buildPricingRequest :: Connection -> FR.SessionId -> ProductPricingRequestDTO
--buildPricingRequest conn sess = do
--    f <- getFlowForSession conn sess
--    let product = find (product f
    
data CoveredLifeType =
    MAIN
    | SPOUSE
    | CHILD
    | PARENT
    | EXTENDED_FAMILY deriving (Read, Show, Generic)

instance ToJSON CoveredLifeType

data Gender =
    MALE |
    FEMALE deriving (Read, Show, Generic)

instance ToJSON Gender

data ProductCoveredLifeDTO = ProductCoveredLifeDTO 
    { age :: Int
    , correlationId :: String
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

data ProductPlanName = 
    UNIVERSITY |
    HOME |
    CAR |
    FOOD deriving (Read, Show, Generic)
    --VANILLA_FUNERAL |
    --DIAMOND_PLAN 
    
data PlanDto = PlanDto 
    { planCorrelationId :: String
    , plan :: ProductPlanName
    } deriving (Read, Show, Generic)

data ProductPricingRequestDTO = ProductPricingRequestDTO 
    { product :: String
    , plans :: [PlanDto]
    , coveredLives :: [ProductCoveredLifeDTO]
    , commencementDate :: Int
    , intermediaryId :: Maybe Int
    , leadSourceId :: Int
    } deriving (Read, Show, Generic)


makeQuestionAnswer :: [FR.Flow] -> [Question] -> [(Question, FR.Flow)]
makeQuestionAnswer [] _ = []
makeQuestionAnswer (h:t) qs = 
    let
        q = DL.find (\e -> FR.qid h == F.qid e) qs
    in
        case q of 
            Nothing -> error $ "Could not find question for answer. qid: " ++ FR.qid h  ++ " aid: " ++ FR.aid h
            Just q' -> (q', h) : makeQuestionAnswer t qs

randUUID :: UUID
randUUID =
    let 
        seed = 123
        g0 = mkStdGen seed
        (u1, g1) = random g0
    in u1


getPricing :: Connection -> FR.SessionId -> [Question] -> IO Integer
getPricing conn sess questionFlow = do
    sessionFlow <- getFlowForSession sess conn
    let plan = (filter (\e -> FR.answer_mapping e == "Plan") sessionFlow)
        planDto = [PlanDto (show randUUID) (read . FR.answer_mapping . head $ plan :: ProductPlanName)]
    return 1

handleFlow :: FR.ProductName -> FR.AnswerId -> FR.Answer -> FR.SessionId -> [Question] -> Connection -> IO Text
handleFlow prod aid userAnswer sess questionFlow conn = do
    let answer = trace ("userAnswer: " ++ show userAnswer) F.getAnswerById aid questionFlow
        questionAnswered = getQuestionFromAnswerId (Just aid) questionFlow
        valid = F.validate answer userAnswer
        f = FR.buildFlow Nothing prod (F.qid questionAnswered) (F.description questionAnswered) aid userAnswer (show $ F.answer_mapping answer) (if valid then "VALID" else "INVALID") sess
    _ <- FR.insertFlowAnswer conn f
    case F.answer_type questionAnswered of
        F.Finalise -> buildTemplate (Just aid) (if valid then Nothing else Just "Validation failed") (getProduct $ TL.pack prod) questionFlow
        _ -> buildTemplate (Just aid) (if valid then Nothing else Just "Validation failed") (getProduct $ TL.pack prod) questionFlow

        
