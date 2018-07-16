{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Lib ( myCors) where

import           Network.Wai.Middleware.Cors

myPolicy =
  CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "PUT", "POST", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = True
  , corsRequireOrigin = False
  , corsIgnoreFailures = True
  }

myCors = cors (const $ Just myPolicy)

