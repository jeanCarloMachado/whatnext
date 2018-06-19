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
import Data.Text (unpack)
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




testAuth wnDir token =  do
  result <- liftIO $ readProcess (wnDir ++ "/" ++ "gateway.sh") ["getEmailByHash", tokenStr] ""
  if Prelude.length result > 3 
  then json (SchedulerOk result)
  else json (ApiError 9 "Token failed")

  where 
  tokenStr = unpack token
 
main = do

  wnDir <- getEnv ("WHATNEXT_SRC")
  scotty 5000 $ do
    middleware simpleCors

    get "/scheduler" $ do
      cookie <- getCookie "Authorization"

      case cookie of 
        Just x -> testAuth wnDir x
        Nothing -> json $ ApiError 6 "Auth cookie needed"


      -- email = check_authorization(request)
      -- my_env = update_environemnt(os.environ.copy(), email)

      -- tiredMode = request.args.get('tiredMode', default = False, type = bool)
      -- if tiredMode:
      --     my_env["TIRED_MODE"] = "1"


      -- cmd = [
      --         BASE_PATH + '/Scheduler',
      --         ]

      -- my_env["TO_JSON"] = "1"
      -- content = subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE).stdout.decode('UTF-8')


      -- return content, 200, {'Content-Type': 'application/json; charset=utf-8'}
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

        -- addHeader $ Text.concat ["Set-Cookie: Authorization=", (Text.pack result),"; Domain=.thewhatnext.net; Path=/"]
        json (AuthResult (result) )
    notFound $ do
      text "Invalid route"
