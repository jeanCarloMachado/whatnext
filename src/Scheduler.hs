{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Process
import Data.Aeson
import Data.ByteString.Lazy.Char8
import GHC.Generics
import Data.List



main = do
        content <- readProcess "./conf2json.sh" [] ""
        let byteVersion = pack content
        d <- (decode <$>  (return byteVersion)) :: IO (Maybe [Subject])
        case d of
          Just ps -> Prelude.putStrLn $ "The result is: " ++ "aa"
          _ -> Prelude.putStrLn "error"

data Subject =
    Subject {
       name :: String
       , priority :: Int
       , complexity :: Int
    } deriving (Show,Generic)


instance FromJSON Subject
instance ToJSON Subject
