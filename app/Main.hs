{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty (scotty, get, html, request, params, post)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find)
import Flow (getQuestionFlow)
import Network.Wai (Request(requestBody))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (getOrg, buildTemplate)

port :: Int
port = 3000

dbName :: String
dbName = "platform"

searchParam :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
searchParam p ps = case snd <$> find (\e -> fst e == p) ps of
    Nothing -> error $ "No param found for: " ++ show p
    Just p' -> p'

main :: IO ()
main = do
    questionFlow <- getQuestionFlow 
    scotty port $ do
        get "/:org" $ do
            ps <- params
            let o = searchParam "org" ps
            t <- trace ("org: " ++ show o ++ "\n") liftIO $ buildTemplate Nothing (getOrg o) questionFlow
            html $ trace (show t) TL.fromStrict t
        post "/:org/answer/:aid" $ do
            ps <- params
            r <- request
            b <- liftIO $ requestBody r
            let o = searchParam "org" ps
                org' = getOrg o
                aid = searchParam "aid" ps
            t <- trace ("request body:" ++ show b ++ "\n") liftIO $ buildTemplate (Just $ TL.unpack aid) org' questionFlow
            html $ trace (show t) TL.fromStrict t
