
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
import Data.List (sortBy)
import Data.Function (on)


main = do
        sourceDirectory <- getEnv("WHATNEXT_SRC")

        (exitCode, history, stderr) <- readProcessWithExitCode (sourceDirectory ++ "/log.sh") ["--json"] ""

        decodedHistory <- (eitherDecode <$> return history) :: IO (Either String [StudyEntry])
        currentTime <- getCurrentTime

        case decodedHistory of
            Left msg ->
                putStr msg
            Right list ->
                -- print  $  encode <$> list
                putStr $ getResultStr list currentTime
                -- print $ formatTime defaultTimeLocale "%V" (date( list !! 0 ))




getResultStr list currentTime =
    donesThisWeekCount ++ "\n" ++ top5Str

    where doneThisWeekList = getDoneThisWeek currentTime list
          donesThisWeekCount = (++) "Done this week: " $ show  $ length $ doneThisWeekList
          topDone =  sortBy (flip compare `on` snd) $ foldl (\a b -> accOrCreate a b) [] doneThisWeekList
          top5 = take 5 topDone
          top5Names = foldl (\a b -> a  ++ (fst b) ++ ", ") "" top5
          top5Str = "Top five: " ++ top5Names

getDoneThisWeek currentDate list =
    filter (\e -> (formatTime defaultTimeLocale "%V" (date e)) == (formatTime defaultTimeLocale "%V" currentDate)) list


accOrCreate resultList studyEntry =
    case filter (\e -> (fst e) == (subject studyEntry)) resultList of
        (x:_) ->
           (fst x, 1 + snd x) : delete x resultList
        _ ->
            ((subject studyEntry), 1) : resultList


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

        let date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" dateStr :: UTCTime

        return (StudyEntry subject date)

instance ToJSON StudyEntry
