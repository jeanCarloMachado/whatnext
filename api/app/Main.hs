{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Crypto.Hash

import Data.ByteString.UTF8



data Credentials = Credentials { login :: String, password :: String } deriving (Generic, Show)
instance ToJSON Credentials
instance FromJSON Credentials

data AuthResult = AuthResult { authHash :: String } deriving (Generic, Show)
instance ToJSON AuthResult
instance FromJSON AuthResult

main = scotty 5000 $ do
  get "/schduler" $ do
    json [(0::Int)..10]
  post "/login" $ do
      credentials <- jsonData :: ActionM Credentials

      let loginAsByte = fromString (login credentials)
          passwordAsByte = fromString (password credentials)
          ctx = hashInitAlg SHA256
          ctx1 = hashUpdate ctx loginAsByte
          ctx2 = hashUpdate ctx1 passwordAsByte
          end = hashFinalize ctx2

      json (AuthResult (toString $ digestToHexByteString end) )
  notFound $ do
    text "Invalid route"
