{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import System.Process
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack, pack)
import GHC.Generics
import Data.List
import Prelude
import Data.Ord (comparing)
import System.Environment


main = do
        currentDirectory <- getEnv("WHATNEXT_SRC")

        whatnextConf <- readProcess (currentDirectory ++ "/conf2json.sh") [] ""
        tiredModeVal <- lookupEnv "TIRED_MODE"
        timePerSubject <- readProcess (currentDirectory ++ "/timePerSubject.py") [] ""

        let whatnextConfAsByte = pack whatnextConf
            tiredMode = tiredModeValToBool tiredModeVal
            timePerSubjectAsByte = pack timePerSubject

        decodedWhatnextConf <- (decode <$> return whatnextConfAsByte) :: IO (Maybe [Subject])

        let decodedTimePerSubject = (decode timePerSubjectAsByte) :: (Maybe [(TimePerSubject)])
            timePerSubjectList = extractTimePerSubject decodedTimePerSubject

        case decodedWhatnextConf of
          Just subjects ->
              do
              subjectsWithDays <- mapM (getDaysSinceLastStudy currentDirectory) subjects

              let finalSubjects = sortSubjects $ computeWeights tiredMode $  applyTimeAlreadyInvested timePerSubjectList subjectsWithDays
              putStrLn  $ mountJson finalSubjects
          _ ->
              putStrLn "error while decoding subjects"


-- subjects transformations

applyTimeAlreadyInvested timePerSubject subjects =
    map (\subject -> applyTimeAlreadInvestedForSubject timePerSubject subject ) subjects

applyTimeAlreadInvestedForSubject timePerSubject subject =
    case resultMatch of
    (x:_) ->
        subject { timeAlreadyInvested = (snd x) }
    _ ->
        subject

    where
        resultMatch = filter (\timeSubject -> (fst timeSubject) == (name subject)) timePerSubject


extractTimePerSubject maybeList =
    case maybeList of
    Just x ->
        x
    _ ->
        []


getDaysSinceLastStudy currentDirectory subject  = do
    daysSinceLastStudy <- readProcess (currentDirectory ++ "/gateway.sh") ["daysSinceLastStudy", name subject] ""
    let daysInt = daysToInt daysSinceLastStudy
    return (subject { daysSinceLastStudy = daysInt})

tiredModeValToBool Nothing  =
    False
tiredModeValToBool _ =
    True

daysToInt "" = 0
daysToInt x = read x :: Int

sortSubjects subjects =
    reverse $ sortBy (comparing weight) subjects




-- subjects calculus

computeWeights tiredMode subjects =
    map (\subject -> subject { weight =  computeWeight tiredMode subject}) subjects

computeWeight tiredMode subject =
    case tiredMode of
    True ->
         baseCalculus / regularizedComplexity
    False ->
        baseCalculus

    where  --regularize values between 0 and 1
          regularizedPriority = (priority subject) / 100
          regularizedComplexity = (complexity subject) / 100
          floatDaysSinceLastStudy = fromIntegral $ daysSinceLastStudy subject
          daysSinceLastStudyQuadratic = (floatDaysSinceLastStudy ** 2) / reasonableMaxDaysWithoutDoing (floatDaysSinceLastStudy ** 2)
          iTimeAlreadyInvested = fromIntegral $ (timeAlreadyInvested subject)
          regualarizedTimeAlreadyInvested = iTimeAlreadyInvested / reasonableMaxTimeStudied (iTimeAlreadyInvested)

         -- apply weights to each value
          weightedComplexity = (regularizedComplexity * 0.2)
          weightedPriority = (regularizedPriority * 0.5)
          weightenedDaysSinceLastStudies  = (daysSinceLastStudyQuadratic * 1)
          weightenedTimeAlreadyInvested = (regualarizedTimeAlreadyInvested * 0.3)

          --calculus
          baseCalculus = (weightedComplexity + weightedPriority + weightenedDaysSinceLastStudies)  / (3 + weightenedTimeAlreadyInvested)

reasonableMaxTimeStudied val =
    -- a reasonable well study has at least 20 hours
    if val > (20 * 60) then
        val
    else
        20 * 60

reasonableMaxDaysWithoutDoing val =
    if val > (365 / 4) then
        val
    else
        365.0 / 4


-- entities
data Subject =
    Subject {
       name :: String
       , priority :: Float
       , complexity :: Float
       , weight :: Float
       , daysSinceLastStudy :: Int
       , objective :: String
       , whatToDoNext :: String
       , timeAlreadyInvested :: Int
    } deriving (Show,Generic)


type TimePerSubject =  (String, Int)

-- json decode
mountJson :: [Subject] -> String
mountJson subjects =
    "[" ++ (intercalate "," (Data.List.map unpack (encode <$> subjects)) ) ++ "]"



instance FromJSON Subject where
  parseJSON = withObject "subject" $ \o -> do
    name <- o .: "name"
    priority <- o .: "priority"
    complexity <- o .: "complexity"
    objective <- o .: "objective"
    whatToDoNext <- o .: "whatToDoNext"
    return (Subject name priority complexity 0 0 objective whatToDoNext 0 )

instance ToJSON Subject where
  toJSON Subject{..} = object [
    "name"    .= name
    , "priority" .= priority
    , "complexity" .= complexity
    , "weight" .= weight
    , "days_since_last_study" .= daysSinceLastStudy
    , "objective" .= objective
    , "whatToDoNext" .= whatToDoNext
    , "time_already_invested" .= timeAlreadyInvested
    ]
