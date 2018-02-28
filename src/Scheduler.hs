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
import Data.Ord (comparing)
import System.Environment

maxDaysWithoutDoing = 365.0

main = do
        currentDirectory <- getEnv("WHATNEXT_SRC")
        content <- readProcess (currentDirectory ++ "/conf2json.sh") [] ""
        let byteVersion = pack content
        d <- (decode <$> return byteVersion) :: IO (Maybe [Subject])
        case d of
          Just subjects ->
              do
              completeSubjects <- mapM  (getDaysSinceLastStudy currentDirectory) subjects
              let finalSubjects = rollbackValues $ sortSubjects $ computeWeights $ regularizeValues completeSubjects
              putStrLn  $ mountJson finalSubjects
          _ ->
              putStrLn "error"


sortSubjects subjects =
    reverse $ sortBy (comparing weight) subjects


computeWeights :: [Subject] -> [Subject]
computeWeights subjects =
    map (\subject -> subject { weight =  computeWeight subject}) subjects

computeWeight subject =
    (iPriority + (iDaysSinceLast / maxDaysWithoutDoing) + iComplexity / 3)

    where iDaysSinceLast = fromIntegral $ daysSinceLastStudy subject
          iWeight = (weight subject)
          iPriority = (priority subject)
          iComplexity = (complexity subject)


regularizeValues subjects =
    map (\subject -> subject { priority = (priority subject) / 100, complexity = (complexity subject) / 100} ) subjects

rollbackValues subjects =
    map (\subject -> subject { priority = 100, complexity = 100}) subjects

mountJson :: [Subject] -> String
mountJson subjects =
    "[" ++ (intercalate "," (Data.List.map unpack (encode <$> subjects)) ) ++ "]"

getDaysSinceLastStudy currentDirectory subject  = do
    daysSinceLastStudy <- readProcess (currentDirectory ++ "/gateway.sh") ["daysSinceLastStudy", name subject] "" 
    let daysInt = daysToInt daysSinceLastStudy
    return (subject { daysSinceLastStudy = daysInt})


daysToInt "" = 0
daysToInt x = read x :: Int


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
