{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Crypto.Hash
import           System.Environment
import           System.Process
import Data.ByteString.UTF8
import Data.Text as Text
import Data.List (concat)
import Web.Cookie
import Web.Scotty.Cookie
import Network.Wai.Middleware.Cors
import Data.Text as T
import Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)


data ApiError = ApiError {status:: Int, message:: String} deriving (Generic, Show)
instance ToJSON ApiError
instance FromJSON ApiError

data SchedulerOk = SchedulerOk {content :: String} deriving (Generic, Show)
instance ToJSON SchedulerOk
instance FromJSON SchedulerOk

data Credentials = Credentials { login :: String, password :: String } deriving (Generic, Show)
instance ToJSON Credentials
instance FromJSON Credentials

data AuthResult = AuthResult { authHash :: String } deriving (Generic, Show)
instance ToJSON AuthResult
instance FromJSON AuthResult


testAuth wnDir token = do
  result <- readProcess (wnDir ++ "/" ++ "gateway.sh") ["getEmailByHash", tokenStr] ""
  if Prelude.length result > 3
  then return (Just result)
  else return Nothing
  where
  tokenStr = T.unpack token


procX f wnDir = do
  token <- liftIO $ f
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "Scheduler") [] ""
  case token of
    Just x -> text $ LT.pack result
    Nothing -> json (ApiError 9 "token invalid")


main = do

  wnDir <- getEnv ("WHATNEXT_SRC")
  scotty 5000 $ do
    middleware simpleCors

    get "/scheduler" $ do
      cookie <- getCookie "Authorization"
      Web.Scotty.setHeader "Content-Type" "application/json"
      case cookie of
        Just x -> procX (testAuth wnDir x) wnDir
        Nothing -> json $ ApiError 6 "Auth cookie needed"

    post "/login" $ do
        credentials <- jsonData :: ActionM Credentials

        let loginAsByte = fromString (login credentials)
            passwordAsByte = fromString (password credentials)
            ctx = hashInitAlg SHA256
            ctx1 = hashUpdate ctx loginAsByte
            ctx2 = hashUpdate ctx1 passwordAsByte
            end = hashFinalize ctx2
            result = toString $ digestToHexByteString end
            cookie = defaultSetCookie { setCookieName = "Authorization", setCookieValue = (fromString result), setCookiePath = (Just (fromString "/")), setCookieDomain = (Just $ fromString ".thewhatnext.net") }

        setCookie cookie

        json (AuthResult (result) )
    notFound $ do
      text "Invalid route"
