{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty (scotty, get, html, params, post, body)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find, Foldable (product))
import qualified Flow as F (getQuestionFlow, getAnswerById, validate) 
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (buildTemplate, getProduct)
import Data.List (elemIndex)
import Data.Text.Internal.Encoding.Utf32 (validate)

port :: Int
port = 3000

dbName :: String
dbName = "platform"

getParam :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
getParam p ps = case snd <$> find (\e -> fst e == p) ps of
    Nothing -> error $ "No param found for: " ++ show p
    Just p' -> p'

trimChar :: Char -> String -> String
trimChar c str = reverse $ dropper $ reverse $ dropper str
    where dropper = dropWhile (==c) 

splitAtAndDrop :: String -> Char -> [String]
splitAtAndDrop [] _ = []
splitAtAndDrop xs c = 
    let xs' = dropWhile (== c) xs
    in case elemIndex c xs' of
        Nothing -> [xs']
        Just i -> take i xs' : splitAtAndDrop (drop i xs') c

bodyPairs :: String -> [(String, String)]
bodyPairs b = 
    let bodyPrs = splitAtAndDrop b '&'
    in map (\e -> 
        let pr = splitAtAndDrop e '='
        in if length pr == 2
           then (head pr, pr !!1)
           else trace ("No '=' in body pair " ++ show pr) (head pr, "")
        ) bodyPrs

parseFromBody :: String -> String -> Maybe String
parseFromBody bod key = 
    let pairs = bodyPairs bod
        pair = find (\e -> fst e == key) pairs
    in fmap snd pair
    
main :: IO ()
main = do
    questionFlow <- F.getQuestionFlow 
    scotty port $ do
        get "/" $ do
            t <- liftIO $ readFile "./templates/home.mustache"
            html $ TL.pack t
        get "/:prod" $ do
            ps <- params
            let p = getParam "prod" ps
            t <- liftIO $ buildTemplate Nothing Nothing (getProduct p) questionFlow
            html $  TL.fromStrict t
        post "/:prod/answer/:aid" $ do
            ps <- params
            b <- body
            let p' = getParam "prod" ps
                aid = getParam "aid" ps
                ans = parseFromBody (trimChar '"' $ show b) "answer"
                answer = F.getAnswerById (show aid) questionFlow
                valid = case answer of
                    Nothing -> False
                    Just answer' -> F.validate answer' (show ans)
                product' = getProduct p'
            t <- if valid
            then trace ("body: " ++ show b ++ "\nans: " ++ show ans) liftIO $ buildTemplate (Just $ TL.unpack aid) Nothing product' questionFlow
            else trace ("body: " ++ show b ++ "\nans: " ++ show ans) liftIO $ buildTemplate (Just $ TL.unpack aid) (Just "Validation failed") product' questionFlow

            html $ TL.fromStrict t
