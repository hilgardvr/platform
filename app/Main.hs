{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty (scotty, get, html, params, post, body)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find)
import Flow (getQuestionFlow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (getOrg, buildTemplate)
import Data.List (elemIndex)
import qualified Data.ByteString.Lazy as BL

port :: Int
port = 3000

dbName :: String
dbName = "platform"

getParam :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
getParam p ps = case snd <$> find (\e -> fst e == p) ps of
    Nothing -> error $ "No param found for: " ++ show p
    Just p' -> p'

trimChars :: Char -> String -> String
trimChars c str = reverse $ dropper $ reverse $ dropper str
    where dropper = dropWhile (==c) 


splitAtAndDrop :: String -> Char -> [String]
splitAtAndDrop [] _ = []
splitAtAndDrop xs c = 
    let
        xs' = dropWhile (== c) xs
    in
        case elemIndex c xs' of
            Nothing -> [xs']
            Just i -> take i xs' : splitAtAndDrop (drop i xs') c

bodyPairs :: String -> [(String, String)]
bodyPairs b = 
    let
        pms = splitAtAndDrop b '&'
    in
        map (\e -> 
            let pr = splitAtAndDrop e '='
            in if length pr == 2
               then (head pr, pr !!1)
               else (head pr, "")
        ) pms

parseFromBody :: String -> String -> Maybe String
parseFromBody bod key = 
    let
        pairs = bodyPairs bod
        pair = find (\e -> fst e == key) pairs
    in
        fmap snd pair
    
main :: IO ()
main = do
    questionFlow <- getQuestionFlow 
    scotty port $ do
        get "/:org" $ do
            ps <- params
            let o = getParam "org" ps
            t <- liftIO $ buildTemplate Nothing (getOrg o) questionFlow
            html $  TL.fromStrict t
        post "/:org/answer/:aid" $ do
            ps <- params
            b <- body
            let o = getParam "org" ps
                ans = parseFromBody (trimChars '"' $ show b) "answer"
                org' = getOrg o
                aid = getParam "aid" ps
            t <- trace ("body: " ++ show b ++ "\nans: " ++ show ans) liftIO $ buildTemplate (Just $ TL.unpack aid) org' questionFlow
            html $ TL.fromStrict t
