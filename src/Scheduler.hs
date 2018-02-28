{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Process
import Data.Aeson
import Data.ByteString.Lazy.Char8
import GHC.Generics



main = do
        content <- readProcess "./conf2json.sh" ["chemistry"] ""
        let byteVersion = pack content
        d <- (decode <$>  (return byteVersion))
        case d of
          Nothing -> Prelude.putStrLn "adsfad"
          Just ps -> Prelude.putStrLn $ "The result is: " ++ (name ps)

data Subject =
    Subject {
       name :: String
    } deriving (Show,Generic)

instance FromJSON Subject

