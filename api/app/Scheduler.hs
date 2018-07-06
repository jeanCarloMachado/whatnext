{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Data.List
import           Data.Ord                   (comparing)
import           GHC.Generics
import           Prelude
import           System.Environment
import           System.Process
import           Data.Time.Calendar (fromGregorian, Day, diffDays)
import Data.Time.Clock (utctDay, getCurrentTime)


main :: IO()
main = do
  currentDirectory <- getEnv ("WHATNEXT_SRC")
  whatnextConf <- readMyProcess currentDirectory "conf2json.sh"
  tiredModeVal <- lookupEnv "TIRED_MODE"
  timePerSubject <- readMyProcess currentDirectory "timePerSubject.py"
  today <- getToday
  let timePerSubjectAsByte = pack timePerSubject
      context = Context (tiredModeValToBool tiredModeVal) today
  subjectsConf <-
    (decode <$> return (pack whatnextConf)) :: IO (Maybe [FutureAction])
  let decodedTimePerFutureAction =
        (decode timePerSubjectAsByte) :: (Maybe [(TimePerFutureAction)])
      timePerSubjectList = extractWithDefault decodedTimePerFutureAction []
  case subjectsConf of
    Just subjects -> do
      subjectsWithDays <- mapM (getDaysSinceLastStudy currentDirectory) subjects
      let finalFutureActions =
            sortByWeight $
            computeWeights context $
            setTimeAlreadyInvested timePerSubjectList subjectsWithDays
      putStrLn $ mountJson finalFutureActions
    _ -> putStrLn "error while decoding subjects"

getToday :: IO (Day) -- :: (year,month,day)
getToday = getCurrentTime >>= return . utctDay

readMyProcess currentDirectory filename = readProcess (currentDirectory ++ "/" ++ filename) [] ""

getDaysSinceLastStudy :: String -> FutureAction -> IO(FutureAction)
getDaysSinceLastStudy currentDirectory subject = do
  daysSinceLastStudy <-
    readProcess
      (currentDirectory ++ "/gateway.sh")
      ["daysSinceLastStudy", name subject]
      ""
  let daysInt = daysToInt daysSinceLastStudy
  return (subject {daysSinceLastStudy = daysInt})

tiredModeValToBool Nothing = False
tiredModeValToBool _       = True

daysToInt "" = 0
daysToInt x  = read x :: Int


sortByWeight :: [FutureAction] -> [FutureAction]
sortByWeight subjects = reverse $ sortBy (comparing weight) subjects

-- subjects calculus
computeWeights :: Context -> [FutureAction] -> [FutureAction]
computeWeights context subjects =
  map (\subject -> subject {weight = computeWeight context subject}) subjects

computeWeight :: Context -> FutureAction -> Float
computeWeight context subject =
  case tiredMode context of
    True  -> weight / regularizedComplexity
    False -> weight
  where
    regularizedPriority = (priority subject) / 100
    regularizedComplexity = (complexity subject) / 100
    floatDaysSinceLastStudy = fromIntegral $ daysSinceLastStudy subject
    daysSinceLastStudyQuadratic =
      (floatDaysSinceLastStudy ** 2) /
      reasonableMaxDaysWithoutDoing (floatDaysSinceLastStudy ** 2)
    iTimeAlreadyInvested = fromIntegral $ (timeAlreadyInvested subject)
    regualarizedTimeAlreadyInvested =
      iTimeAlreadyInvested / reasonableMaxTimeStudied (iTimeAlreadyInvested)

    daysSinceCreation = diffDays (today context) $ creationDate subject
    regulariedDaysSinceCreation = (1 / ((fromIntegral $ daysSinceCreation ^ 2)  + 0.0001))
         -- apply weights to each value
    wComplexity = (regularizedComplexity * 0.2)
    wPriority = (regularizedPriority * 0.5)
    wDaysSinceLastStudies = (daysSinceLastStudyQuadratic * 1)
    wTimeAlreadyInvested = (regualarizedTimeAlreadyInvested * 0.3)
    wDaysSinceCreation = (regulariedDaysSinceCreation * 0.19)
    zeroingFactor = getZeroingFactor (priority subject)

    numberOfFactors = 4
    weight =
      ((wComplexity + wPriority + wDaysSinceLastStudies + wDaysSinceCreation) * zeroingFactor) / (numberOfFactors + wTimeAlreadyInvested)

getZeroingFactor :: Float -> Float
getZeroingFactor priority =
    if priority <= 0
    then 0 :: Float
    else 1 :: Float


type Minutes = Float

reasonableMaxTimeStudied :: Minutes -> Minutes
reasonableMaxTimeStudied val
    -- a reasonable well study has at least 20 hours
 =
  if val > (20 * 60)
    then val
    else 20 * 60

reasonableMaxDaysWithoutDoing val =
  if val > (365 / 4)
    then val
    else 365.0 / 4

-- entities
data FutureAction = FutureAction
  { name                :: String
  , priority            :: Float
  , complexity          :: Float
  , weight              :: Float
  , daysSinceLastStudy  :: Int
  , objective           :: String
  , whatToDoNext        :: String
  , timeAlreadyInvested :: Int
  , creationDate :: Day
  } deriving (Show, Generic)

data Context = Context {
  tiredMode :: Bool,
  today :: Day
} deriving (Show, Generic)

type TimePerFutureAction = (String, Int)

-- json decode
mountJson :: [FutureAction] -> String
mountJson subjects =
  "[" ++ (intercalate "," (Data.List.map unpack (encode <$> subjects))) ++ "]"

instance FromJSON FutureAction where
  parseJSON =
    withObject "subject" $ \o -> do
      name <- o .: "name"
      priority <- o .: "priority"
      complexity <- o .: "complexity"
      objective <- o .: "objective"
      whatToDoNext <- o .: "whatToDoNext"
      creationDate <- o .: "creationDate"
      let day = getDay $ explodeDate creationDate
      return ( FutureAction name priority complexity 0 0 objective whatToDoNext 0 day)

instance ToJSON FutureAction where
  toJSON FutureAction {..} =
    object
      [ "name" .= name
      , "priority" .= priority
      , "complexity" .= complexity
      , "weight" .= weight
      , "days_since_last_study" .= daysSinceLastStudy
      , "objective" .= objective
      , "whatToDoNext" .= whatToDoNext
      , "time_already_invested" .= timeAlreadyInvested
      , "creationDate" .= creationDate
      ]

explodeDate :: String -> [Int]
explodeDate dateStr =
  map (\x -> read x :: Int) array
  where
  array = split '-' dateStr

getDay :: [Int] -> Day
getDay list =
  fromGregorian y m d
  where
    y = fromIntegral $ head list
    m = head $ tail list
    d = head $ tail $ tail list

-- subjects transformations

setTimeAlreadyInvested :: [TimePerFutureAction] -> [FutureAction] -> [FutureAction]
setTimeAlreadyInvested timePerSubject subjects =
  map
    (\subject -> applyTimeAlreadInvestedForFutureAction timePerSubject subject)
    subjects

applyTimeAlreadInvestedForFutureAction :: [TimePerFutureAction] -> FutureAction -> FutureAction
applyTimeAlreadInvestedForFutureAction timePerSubject subject =
  case resultMatch of
    (x:_) -> subject {timeAlreadyInvested = (snd x)}
    _     -> subject
  where
    resultMatch =
      filter
        (\timeFutureAction -> (fst timeFutureAction) == (name subject))
        timePerSubject

extractWithDefault ::  Maybe a -> a -> a
extractWithDefault maybeList def =
  case maybeList of
    Just x -> x
    _      -> def

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s
