{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
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
       , weight :: Float
    } deriving (Show,Generic)


instance FromJSON Subject where
  parseJSON = withObject "subject" $ \o -> do
    name <- o .: "name"
    priority <- o .: "priority"
    complexity <- o .: "complexity"
    return (Subject name priority complexity 0)

instance ToJSON Subject where
  toJSON Subject{..} = object [
    "name"    .= name,
    "priority" .= priority,
    "complexity" .= complexity,
    "weight" .= weight
    ]
