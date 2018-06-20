{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Crypto.Hash
import           System.Environment
import           System.Process
import Data.ByteString.UTF8 as BT
import Data.Text as Text
import Data.List (concat)
import Data.String
import Web.Cookie
import Web.Scotty.Cookie
import Network.Wai.Middleware.Cors
import Data.Text as T
import Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)


main = do
  wnDir <- getEnv ("WHATNEXT_SRC")
  scotty 6000 $ do
    -- middleware myCors
    middleware myCors
    get "/scheduler" $ do
      Web.Scotty.setHeader "Content-Type" "application/json"
      runAuthenticatedService wnDir $ scheduler wnDir

    post "/login" $ do
        credentials <- jsonData :: ActionM Credentials

        let loginAsByte = BT.fromString (email credentials)
            passwordAsByte = BT.fromString (password credentials)
            ctx = hashInitAlg SHA256
            ctx1 = hashUpdate ctx loginAsByte
            ctx2 = hashUpdate ctx1 passwordAsByte
            end = hashFinalize ctx2
            result = toString $ digestToHexByteString end
            cookie = defaultSetCookie { setCookieName = "Authorization", setCookieValue = (BT.fromString result), setCookiePath = (Just (BT.fromString "/")), setCookieDomain = (Just $ BT.fromString ".thewhatnext.net") }

        setCookie cookie

        json (AuthResult (result) )

    get "/detail" $ do
      text "soon"

    notFound $ do
      text "Invalid route"


scheduler wnDir (Right token) = return $ Right token
scheduler wnDir (Left token) = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "Scheduler") [] ""
  return $ Left result

cookieValid wnDir (Right x) = return $ Right x
cookieValid wnDir (Left x) = do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "gateway.sh") ["getEmailByHash", x] ""
  if Prelude.length result > 3
  then return $ Left $ result
  else return $ Right "token invalid"
  where

cookieExistence cookie =
      case cookie of
        Just x -> return $ Left $ T.unpack x
        Nothing -> return $ Right "Auth cookie needed"

printResult (Right x) = json $ ApiError x
printResult (Left x) = text $ LT.pack  x

runAuthenticatedService wnDir service =
      getCookie "Authorization" >>= cookieExistence >>= cookieValid wnDir >>= service >>= printResult


myPolicy = CorsResourcePolicy
    { corsOrigins = (Just (["http://127.0.0.1:3000"], True))
    , corsMethods = ["GET","PUT","POST","DELETE","OPTIONS"]
    , corsRequestHeaders = ["Content-Type","Authorization"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = True
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

myCors = cors (const $ Just myPolicy)

data ApiError = ApiError {message:: String} deriving (Generic, Show)
instance ToJSON ApiError
instance FromJSON ApiError

data SchedulerOk = SchedulerOk {content :: String} deriving (Generic, Show)
instance ToJSON SchedulerOk
instance FromJSON SchedulerOk

data Credentials = Credentials { email :: String, password :: String } deriving (Generic, Show)
instance ToJSON Credentials
instance FromJSON Credentials

data AuthResult = AuthResult { authHash :: String } deriving (Generic, Show)
instance ToJSON AuthResult
instance FromJSON AuthResult

