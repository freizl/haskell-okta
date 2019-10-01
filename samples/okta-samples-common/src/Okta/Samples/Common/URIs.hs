{-# LANGUAGE OverloadedStrings #-}

module Okta.Samples.Common.URIs where

import           Data.Text.Lazy (Text)

authorizeUrl :: Text
authorizeUrl = "/v1/authorize"

tokenUrl :: Text
tokenUrl = "/v1/token"

keyUrl :: Text
keyUrl = "/v1/keys"

userinfoUrl :: Text
userinfoUrl = "/v1/userinfo"
