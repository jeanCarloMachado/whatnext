{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UnicodeSyntax         #-}

import           Control.Monad.IO.Class      (liftIO)
import           Crypto.Hash
import           Data.ByteString.UTF8        as BT
import           Data.List                   (concat)
import           Data.String
import           Data.Text                   as Text
import           Data.Text.Lazy              as LazyText
import           GHC.Generics
import           System.Environment
import           System.Process
import           Web.Cookie
import           Web.Scotty
import           Web.Scotty.Cookie
import           Data.Maybe
import           Lib
import           Data.Aeson                  (FromJSON, ToJSON)

main = do
  wnDir <- getEnv ("WHATNEXT_SRC")
  scotty 3001 $
   do
    middleware Lib.myCors

    post "/signup" $ do
      credentials <- jsonData :: ActionM Credentials
      let authToken = createAuthToken credentials
      liftIO $
        readProcess
          (wnDir ++ "/" ++ "signup.sh")
          [(email credentials), authToken]
          ""
      setAuthEnv (Left $ email credentials)
      liftIO $ readProcess (wnDir ++ "/" ++ "init.sh") [] ""
      Web.Scotty.setHeader "Content-Type" "application/json"
      text "{\"status\": \"success\"}"

    post "/login" $ do
      credentials <- jsonData :: ActionM Credentials
      let authToken = createAuthToken credentials
      json (AuthResult (authToken))

    get "/scheduler" $ do
      params <- params

      setTiredMode params
      Web.Scotty.setHeader "Content-Type" "application/json"
      runAuthenticatedService wnDir $ scheduler wnDir

    get "/detail/:subjectName" $ do
      subjectName <- param "subjectName"
      runAuthenticatedService wnDir $ detail wnDir subjectName

    get "/rm/:subjectName" $ do
      subjectName <- param "subjectName"
      runAuthenticatedService wnDir $ remove wnDir subjectName

    post "/addOrUpdate" $ do
      alterInfo <- jsonData :: ActionM AlterInfo
      runAuthenticatedService wnDir $ alterSubject wnDir alterInfo

    get "/log" $ do
      runAuthenticatedService wnDir $ getHistory wnDir

    post "/done" $ do
      doneInfo <- jsonData :: ActionM DoneInfo
      runAuthenticatedService wnDir $ newDoneEntry wnDir doneInfo

    notFound $ do text "Invalid route"

--- state IO
alterSubject wnDir alterInfo (Right token) = return $ Right token
alterSubject wnDir alterInfo _ = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "alterSubject.sh") infoList ""
  return $ Left result
  where
    alterInfoNew = filterAlterData alterInfo

    infoList =
      [
      name alterInfoNew
      , show $ priority alterInfoNew
      , show $ complexity alterInfoNew
      , whatToDoNext alterInfoNew
      , objective alterInfoNew
      , previousName alterInfoNew
      , fromMaybe "" $ parent alterInfoNew
      ]

filterAlterData subject =
    subject { name = Text.unpack $ Text.strip  $ Text.pack $ name subject }

setTiredMode params =
  case elem ("tiredMode", "True") params of
  True ->
      liftIO $ setEnv "TIRED_MODE" "1"
  False ->
      liftIO $ setEnv "TIRED_MODE" ""

newDoneEntry wnDir doneInfo (Right token) = return $ Right token
newDoneEntry wnDir doneInfo _ = do
  liftIO $ readProcess (wnDir ++ "/" ++ "done.sh") infoList ""
  return $ Left "{\"status\": \"success\"}"
  where
    infoList = [subjectName doneInfo, description doneInfo, show $ duration doneInfo]

remove wnDir subjectName (Right token) = return $ Right token
remove wnDir subjectName _ = do
  liftIO $ readProcess (wnDir ++ "/" ++ "rm.sh") [subjectName] ""
  return $ Left "{\"status\": \"success\"}"

detail wnDir subjectName (Right token) = return $ Right token
detail wnDir subjectName _ = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "detail.py") [subjectName] ""
  return $ Left result

getHistory wnDir (Right token) = return $ Right token
getHistory wnDir _ = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "log.sh") [] ""
  return $ Left result

scheduler wnDir (Right token) = return $ Right token
scheduler wnDir (Left token) = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "Scheduler") [] ""
  return $ Left result

----
cookieValid wnDir (Right x) = return $ Right x
cookieValid wnDir (Left x) = do
  result <-
    liftIO $ readProcess (wnDir ++ "/" ++ "gateway.sh") ["getEmailByHash", x] ""
  if Prelude.length result > 3
    then return $ Left $ result
    else return $ Right "token invalid"
  where



setAuthEnv (Right x) = return $ Right x
setAuthEnv (Left email) = do
  liftIO $
    setEnv "WHATNEXT_CONF" $
    "/data/whatnext/users/" ++ email ++ "/whatnext.conf"
  liftIO $
    setEnv "WHATNEXT_GOALS" $
    "/data/whatnext/users/" ++ email ++ "/whatnext_goals.conf"
  liftIO $
    setEnv "WHATNEXT_HISTORY" $
    "/data/whatnext/users/" ++ email ++ "/whatnext_history"
  return $ Left email

cookieExistence cookie =
  case cookie of
    Just x  -> return $ Left $ LazyText.unpack x
    Nothing -> return $ Right "Auth cookie needed"

printResult (Right x) = json $ ApiError x
printResult (Left x)  = text $ LazyText.pack x

runAuthenticatedService wnDir service =
  header "Authorization" >>= cookieExistence >>= cookieValid wnDir >>=
  setAuthEnv >>=
  service >>=
  printResult

createAuthToken credentials = toString $ digestToHexByteString end
  where
    loginAsByte = BT.fromString (email credentials)
    passwordAsByte = BT.fromString (password credentials)
    ctx = hashInitAlg SHA256
    ctx1 = hashUpdate ctx loginAsByte
    ctx2 = hashUpdate ctx1 passwordAsByte
    end = hashFinalize ctx2


data ApiError = ApiError
  { message :: String
  } deriving (Generic, Show)

instance ToJSON ApiError

instance FromJSON ApiError

data SucessMesssage = SucessMesssage
  { status :: String
  } deriving (Generic, Show)

instance ToJSON SucessMesssage

instance FromJSON SucessMesssage

data SchedulerOk = SchedulerOk
  { content :: String
  } deriving (Generic, Show)

instance ToJSON SchedulerOk

instance FromJSON SchedulerOk

data Credentials = Credentials
  { email    :: String
  , password :: String
  } deriving (Generic, Show)

instance ToJSON Credentials

instance FromJSON Credentials

data AuthResult = AuthResult
  { authHash :: String
  } deriving (Generic, Show)

instance ToJSON AuthResult

instance FromJSON AuthResult

data AlterInfo = AlterInfo
  { name         :: String
  , objective    :: String
  , previousName :: String
  , whatToDoNext :: String
  , complexity   :: Int
  , priority     :: Int
  , parent :: Maybe String
  } deriving (Generic, Show)

instance ToJSON AlterInfo

instance FromJSON AlterInfo

data DoneInfo = DoneInfo
  { description :: String
  , subjectName    :: String
  , duration   :: Int
  } deriving (Generic, Show)

instance ToJSON DoneInfo

instance FromJSON DoneInfo

