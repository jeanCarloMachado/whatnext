
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Prelude
import System.Environment
import Data.Aeson
import System.Process.ByteString.Lazy
import GHC.Generics
import Data.List


main = do
        sourceDirectory <- getEnv("WHATNEXT_SRC")

        (exitCode, history, stderr) <- readProcessWithExitCode (sourceDirectory ++ "/log.sh") ["--json"] ""

        decodedHistory <- (eitherDecode <$> return history) :: IO (Either String [StudyEntry])


        case decodedHistory of
            Left msg ->
                print msg
            Right list ->
                print  "abc"


--entities
-- date, subject
data StudyEntry  = StudyEntry {
    subject :: String
    , date :: String
} deriving (Show,Generic)



--decoder
instance FromJSON StudyEntry
instance ToJSON StudyEntry
