module FlowService
( handleFlow
) where
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import qualified Flow as F
import FlowRepo (getFlow, insertFlowAnswer)
import Database.SQLite.Simple (Connection)
import Flow (getQuestionFromAnswerId)
import Templates (buildTemplate, getProduct)

handleFlow :: String -> String -> String -> Connection -> IO Text
handleFlow prod aid userAnswer conn = do
    questionFlow <- F.getQuestionFlow
    let answer = trace ("userAnswer: " ++ show userAnswer) F.getAnswerById aid questionFlow
        questionAnswered = getQuestionFromAnswerId (Just aid) questionFlow
        valid = F.validate answer userAnswer
        f = getFlow (aid ++ show valid) prod (F.qid questionAnswered) (F.description questionAnswered) aid userAnswer (if valid then "VALID" else "INVALID")
    _ <- insertFlowAnswer conn f
    buildTemplate (Just aid) (if valid then Nothing else Just "Validation failed") (getProduct $ TL.pack prod) questionFlow
        
