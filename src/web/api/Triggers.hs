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
import qualified Data.Text.Lazy as L
import qualified Data.Text as NL
import           System.Process.ByteString.Lazy
import           Network.Mail.Mime
import           Network.Mail.Client.Gmail

main = do
  sourceDirectory <- getEnv ("WHATNEXT_SRC")
  smtpPassword <- getEnv ("SMTP_PASSWORD")
  destination <- getEnv ("WHATNEXT_USER")
  (_, history, _) <-
    readProcessWithExitCode (sourceDirectory ++ "/log.sh") ["--json"] ""
  decodedHistory <-
    (eitherDecode <$> return history) :: IO (Either String [StudyEntry])
  currentTime <- getCurrentTime
  case decodedHistory of
    Right list ->
        deliver smtpPassword destination list currentTime
    Left error   -> putStr error

deliver smtpPassword destination list currentTime =
    sendGmail "contato@jeancarlomachado.com.br" (L.pack smtpPassword)  (Address (Just "") "contato@jeancarlomachado.com.br") [Address (Just "") (NL.pack destination)] [] [] "Whatnext - Week status" (L.pack content) [] 10000000
    where content = getResultStr list currentTime

getResultStr list currentTime =
  donesThisWeekCount ++
  "\n" ++
  timeInvested ++
  "\n" ++
  allTimeAverage ++ "\n" ++ sessionsToIncreaseAverageStr ++ "\n" ++ topNamesStr
  where
    timeInvested =
      "Time invested: " ++
      (show $ (*) 50 $ length doneThisWeekList) ++ " minutes"
    allTimeAverage =
      "Average sessions per week: " ++ (show averageSessionsPerWeek)
    donesThisWeekCount =
      (++) "Sessions this week: " $ show $ length $ doneThisWeekList
    topNamesStr = "Top five: " ++ names
    names = foldl (\a b -> a ++ (fst b) ++ ", ") "" topDone
    doneThisWeekList = getDoneThisWeek currentTime list
    totalAlreadyDone = length list
    weeksOfUse = floor $ weeksBetweenDates currentTime $ date $ last list
    averageSessionsPerWeek =
      floor $ (realToFrac $ totalAlreadyDone) / (realToFrac weeksOfUse)
    minToIncreaseAverage =
      (-)
        (sessionsToIncreaseAverage
           totalAlreadyDone
           (weeksOfUse + 1)
           averageSessionsPerWeek)
        totalAlreadyDone
    sessionsToIncreaseAverageStr =
      "Min sessions to increase average by 1: " ++ show minToIncreaseAverage
    topDone = take 5 $ getSubjectsOrderedByEffort doneThisWeekList

sessionsToIncreaseAverage :: Int -> Int -> Int -> Int
sessionsToIncreaseAverage sessionsCount weeks average
  | floor (realToFrac (sessionsCount + 1) / realToFrac weeks) > average =
    sessionsCount + 1
  | otherwise = sessionsToIncreaseAverage (sessionsCount + 1) weeks average

weeksBetweenDates x y = weeks
  where
    weeks = days / 7
    days = hours / 24
    hours = minutes / 60
    minutes = ellapsedSeconds / 60
    ellapsedSeconds = toRational $ diffUTCTime x y

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
