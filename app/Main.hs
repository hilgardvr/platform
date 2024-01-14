{-# LANGUAGE OverloadedStrings #-}

module Main 
( main 
) where
import Web.Scotty (scotty, get, html, params, post, body, redirect)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find)
import qualified Flow as F (getQuestionFlow) 
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (buildTemplate, getProduct)
import Data.List (elemIndex)
import Migrations (openDatabase)
import FlowService (handleFlow)
import Web.Scotty.Cookie (makeSimpleCookie, getCookie, setCookie)
import qualified Data.Text as T


main :: IO ()
main = do
    questionFlow <- F.getQuestionFlow 
    conn <- openDatabase dbName
    scotty port $ do
        get "/" $ do
            t <- liftIO $ readFile "./templates/home.mustache"
            html $ TL.pack t
        get "/:prod" $ do
            ps <- params
            let p = getParam "prod" ps
                c = makeSimpleCookie "platform" "demo"
            t <- liftIO $ buildTemplate Nothing Nothing (getProduct p) questionFlow
            setCookie c
            html $ TL.fromStrict t
        post "/:prod/answer/:aid" $ do
            ps <- params
            b <- body
            cookie <- getCookie "platform"
            case cookie of
                Nothing -> redirect "/"
                Just c -> do
                    let p' = trace ("cookie: " ++ show c) getParam "prod" ps
                        aid = getParam "aid" ps
                        userAnswer = parseFromBody (trimChar '"' $ show b) "answer"
                    t <- liftIO $ handleFlow (TL.unpack p') (TL.unpack aid) userAnswer (T.unpack c) questionFlow conn
                    html $ TL.fromStrict t

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

parseFromBody :: String -> String -> String
parseFromBody bod key = 
    let pairs = bodyPairs bod
        pair = find (\e -> fst e == key) pairs
    in case pair of 
        Just pr -> snd pr
        Nothing -> error $ "No key found for: " ++ key
