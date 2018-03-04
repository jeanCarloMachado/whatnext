{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Data.Aeson
import           Data.Char
import           Data.Function                  (on)
import           Data.List
import           Data.List                      (sortBy)
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Format
import           GHC.Generics
import           Prelude
import           System.Environment
import           System.Process.ByteString.Lazy

main = do
  sourceDirectory <- getEnv ("WHATNEXT_SRC")
  (_, history, _) <-
    readProcessWithExitCode (sourceDirectory ++ "/log.sh") ["--json"] ""
  decodedHistory <-
    (eitherDecode <$> return history) :: IO (Either String [StudyEntry])
  currentTime <- getCurrentTime
  case decodedHistory of
    Left msg   -> putStr msg
    Right list -> putStr $ getResultStr list currentTime

getResultStr list currentTime =
  donesThisWeekCount ++
  "\n" ++ timeInvested ++ "\n" ++ allTimeAverage ++ "\n" ++ "\n" ++ topNamesStr
  where
    timeInvested =
      "Time invested: " ++
      (show $ (*) 50 $ length doneThisWeekList) ++ " minutes"
    allTimeAverage =
      "Average sessions per week: " ++ (show $ floor averageTime)
    doneThisWeekList = getDoneThisWeek currentTime list
    totalAlreadyDone = realToFrac $ length list
    weeksOfUse = (weeksBetweenDates currentTime (date (last list))) 
    averageTime = totalAlreadyDone / weeksOfUse

    donesThisWeekCount =
      (++) "Sessions this week: " $ show $ length $ doneThisWeekList
    topDone = take 5 $ getSubjectsOrderedByEffort doneThisWeekList
    names = foldl (\a b -> a ++ (fst b) ++ ", ") "" topDone
    topNamesStr = "Top five: " ++ names

weeksBetweenDates x y =
    weeks
    where
        weeks = days / 7
        days = hours / 24
        hours = minutes / 60
        minutes = ellapsedSeconds / 60
        ellapsedSeconds =  toRational $ diffUTCTime x y

getSubjectsOrderedByEffort doneThisWeekList =
  sortBy (flip compare `on` snd) $
  foldl (\a b -> accOrCreate a b) [] doneThisWeekList

getDoneThisWeek currentDate list =
  filter (\e -> (weekNumber (date e)) == (weekNumber currentDate)) list

weekNumber date = formatTime defaultTimeLocale "%Y-%V" date

accOrCreate resultList studyEntry =
  case filter (\e -> (fst e) == (subject studyEntry)) resultList of
    (x:_) -> (fst x, 1 + snd x) : delete x resultList
    _     -> ((subject studyEntry), 1) : resultList

--entities
-- date, subject
data Statistics = Statistics
  { doneThisWeek :: Int
  }

data StudyEntry = StudyEntry
  { subject :: String
  , date    :: UTCTime
  } deriving (Show, Generic)

--decoder
instance FromJSON StudyEntry where
  parseJSON =
    withObject "studyentry" $ \o -> do
      subject <- o .: "subject"
      dateStr <- o .: "date"
      let date =
            parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" dateStr :: UTCTime
      return (StudyEntry subject date)

instance ToJSON StudyEntry
