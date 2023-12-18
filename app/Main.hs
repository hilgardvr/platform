{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty (scotty, get, param, html, request, params)
import Text.Mustache (automaticCompile, object, ToMustache (toMustache), substitute)
import Text.Mustache.Types ((~>))
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace)
import Data.Foldable (find)
import Migrations (openDatabase, runMigrations)


port :: Int
port = 3000

newtype Organisation = Organisation { name :: TL.Text }

instance ToMustache Organisation where
    toMustache (Organisation { name = n}) = 
        object ["name" ~> n]
        --
data Question = Question {
    org :: Organisation
    , qid :: TL.Text
}

instance ToMustache Question where
    toMustache q = object 
        [
            "org" ~> org q
            , "qid" ~> qid q
        ]


searchParam :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
searchParam p ps = case snd <$> find (\e -> fst e == p) ps of
    Nothing -> error $ "No param found for: " ++ show p
    Just p' -> p'

main :: IO ()
main = 
    let 
        searchSpace = ["./templates"]
        templateName = "index.html"
        dbName = "platform"
    in do
        conn <- openDatabase dbName
        _ <- runMigrations conn
        compiled <- automaticCompile searchSpace templateName
        case compiled of 
            Left err -> print err
            Right tmpl ->
                scotty port $ do
                    get "/:org" $ do
                        p <- param "org"
                        let o = trace (show p) Organisation p
                            t = substitute tmpl o
                        html $ trace ("templ: " ++ show t) $ TL.fromStrict t
                    get "/:org/:qid" $ do
                        r <- request
                        --b <- liftIO $ requestBody r
                        ps <- params
                        let o = searchParam "org" ps
                            i = searchParam "qid" ps
                            q = Question {
                                    org = Organisation o
                                    , qid = i
                                }
                            t = substitute tmpl q
                        html $ trace ("templ: " ++ show t) $ TL.fromStrict t
