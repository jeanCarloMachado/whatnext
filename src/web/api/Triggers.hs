
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
import Data.Char
import Data.Time
import Data.Time.Clock
import Data.Time.Format


main = do
        sourceDirectory <- getEnv("WHATNEXT_SRC")

        (exitCode, history, stderr) <- readProcessWithExitCode (sourceDirectory ++ "/log.sh") ["--json"] ""

        decodedHistory <- (eitherDecode <$> return history) :: IO (Either String [StudyEntry])
        currentTime <- getCurrentTime

        case decodedHistory of
            Left msg ->
                print msg
            Right list ->

                -- print  $  encode <$> list
                print $ map (\e -> formatTime defaultTimeLocale "%Y-%m-%d" (date e)) list
                -- print  $ (++) "Done this week: " $ show  $ getDoneThisWeek currentTime list
                -- print $ formatTime defaultTimeLocale "%V" (date( list !! 0 ))


getDoneThisWeek currentDate list =
    length thisWeekMembers
    where 
        thisWeekMembers = filter (\e -> (formatTime defaultTimeLocale "%V" (date e)) == (formatTime defaultTimeLocale "%V" currentDate)) list


data Statistics = Statistics {
    doneThisWeek :: Int
}

--entities
-- date, subject
data StudyEntry  = StudyEntry {
    subject :: String
    , date :: UTCTime
} deriving (Show,Generic)



--decoder
instance FromJSON StudyEntry where
    parseJSON = withObject "studyentry" $ \o -> do
        subject <- o .: "subject"
        dateStr <- o .: "date"

        let date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%s" dateStr :: UTCTime

        return (StudyEntry subject date)

instance ToJSON StudyEntry
