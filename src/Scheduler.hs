{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Process
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack, pack)
import GHC.Generics
import Data.List
import Prelude



main = do
        content <- readProcess "./conf2json.sh" [] ""
        let byteVersion = pack content
        d <- (decode <$>  (return byteVersion)) :: IO (Maybe [Subject])
        case d of
          Just ps -> putStrLn (mountJson ps)
          _ -> Prelude.putStrLn "error"


mountJson subjects =
    ("[" ++ (intercalate "," (Data.List.map unpack (encode <$> subjects)) ) ++ "]")



data Subject =
    Subject {
       name :: String
       , priority :: Int
       , complexity :: Int
    } deriving (Show,Generic)


instance FromJSON Subject
instance ToJSON Subject
