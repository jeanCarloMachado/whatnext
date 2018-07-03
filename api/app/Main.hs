{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UnicodeSyntax         #-}

import           Control.Monad.IO.Class      (liftIO)
import           Crypto.Hash
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.ByteString.UTF8        as BT
import           Data.List                   (concat)
import           Data.String
import           Data.Text                   as Text
import           Data.Text                   as T
import           Data.Text.Lazy              as LT
import           GHC.Generics
import           Network.Wai.Middleware.Cors
import           System.Environment
import           System.Process
import           Web.Cookie
import           Web.Scotty
import           Web.Scotty.Cookie

main = do
  wnDir <- getEnv ("WHATNEXT_SRC")
  scotty 3001 $
    -- middleware myCors
   do
    middleware myCors
    get "/scheduler" $ do
      Web.Scotty.setHeader "Content-Type" "application/json"
      runAuthenticatedService wnDir $ scheduler wnDir
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
          cookie =
            defaultSetCookie
            { setCookieName = "Authorization"
            , setCookieValue = (BT.fromString authToken)
            , setCookiePath = (Just (BT.fromString "/"))
            , setCookieDomain = (Just $ BT.fromString ".thewhatnext.net")
            }
      setCookie cookie
      json (AuthResult (authToken))
    get "/detail/:subjectName" $ do
      subjectName <- param "subjectName"
      runAuthenticatedService wnDir $ detail wnDir subjectName
    get "/rm/:subjectName" $ do
      subjectName <- param "subjectName"
      runAuthenticatedService wnDir $ remove wnDir subjectName
    post "/addOrUpdate" $ do
      alterInfo <- jsonData :: ActionM AlterInfo
      runAuthenticatedService wnDir $ alter wnDir alterInfo
    get "/log" $ do runAuthenticatedService wnDir $ getLogs wnDir
    post "/done/:subjectName" $ do
      subjectName <- param "subjectName"
      doneInfo <- jsonData :: ActionM DoneInfo
      runAuthenticatedService wnDir $ done wnDir subjectName doneInfo
    notFound $ do text "Invalid route"

--- state IO
alter wnDir alterInfo (Right token) = return $ Right token
alter wnDir alterInfo _ = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "alterSubject.sh") infoList ""
  return $ Left result
  where
    infoList =
      [ name alterInfo
      , show $ priority alterInfo
      , show $ complexity alterInfo
      , whatToDoNext alterInfo
      , objective alterInfo
      , previousName alterInfo
      ]

done wnDir subjectName doneInfo (Right token) = return $ Right token
done wnDir subjectName doneInfo _ = do
  liftIO $ readProcess (wnDir ++ "/" ++ "done.sh") infoList ""
  return $ Left "{\"status\": \"success\"}"
  where
    infoList = [subjectName, description doneInfo, followup doneInfo]

remove wnDir subjectName (Right token) = return $ Right token
remove wnDir subjectName _ = do
  liftIO $ readProcess (wnDir ++ "/" ++ "rm.sh") [subjectName] ""
  return $ Left "{\"status\": \"success\"}"

detail wnDir subjectName (Right token) = return $ Right token
detail wnDir subjectName _ = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "detail.py") [subjectName] ""
  return $ Left result

getLogs wnDir (Right token) = return $ Right token
getLogs wnDir _ = do
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
    Just x  -> return $ Left $ T.unpack x
    Nothing -> return $ Right "Auth cookie needed"

printResult (Right x) = json $ ApiError x
printResult (Left x)  = text $ LT.pack x

runAuthenticatedService wnDir service =
  getCookie "Authorization" >>= cookieExistence >>= cookieValid wnDir >>=
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

myPolicy =
  CorsResourcePolicy
  { corsOrigins =
      (Just (["http://127.0.0.1:3000", "https://app.thewhatnext.net"], True))
  , corsMethods = ["GET", "PUT", "POST", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = True
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

myCors = cors (const $ Just myPolicy)

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
  } deriving (Generic, Show)

instance ToJSON AlterInfo

instance FromJSON AlterInfo

data DoneInfo = DoneInfo
  { description :: String
  , followup    :: String
  } deriving (Generic, Show)

instance ToJSON DoneInfo

instance FromJSON DoneInfo
