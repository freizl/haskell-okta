{{-# LANGUAGE OverloadedStrings #-}

module Specs.Internal where

import           Data.Aeson                (encode)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.String
import qualified Data.Text.Lazy            as TL
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai.Test          (SResponse)
import           Prelude                   hiding (exp)
import           Test.Hspec.Wai

import           Okta.Samples.Template
import           Okta.Samples.Types


overviewPage :: IO String
overviewPage = TL.unpack <$> tplS "home" Nothing sampleConfig

loginCustomPage :: IO String
loginCustomPage = TL.unpack <$> tplS "login" Nothing sampleConfig

profilePage :: IO String
profilePage = TL.unpack <$> tplS "profile" (Just sampleCookieUser) sampleConfig


getWithUserCookie :: ByteString -> WaiSession SResponse
getWithUserCookie path = getWithCookie path userCookie

getWithCookie :: ByteString -> ByteString -> WaiSession SResponse
getWithCookie path cookies = request methodGet path [(hCookie, cookies)] ""

userCookie :: ByteString
userCookie = "user=" `BS.append` BSL.toStrict (encode sampleCookieUser)

sampleCookieUser :: UserInfo
sampleCookieUser = UserInfo
  { _userInfoSub = "aecabeuc012"
  , _userInfoName = "Test UserA"
  , _userInfoGivenName = "Test"
  , _userInfoFamilyName = "UserA"
  , _userInfoEmail = "tu2@test.com"
  , _userInfoEmailVerified = True
  , _userInfoZoneinfo = "Test Zone"
  }


sampleConfig :: Config
sampleConfig = Config o 8080
  where o = OIDC { _configScope = "openid profile email"
                 , _issuer = "https://rain.okta1.com/oauth2/default"
                 , _clientId = "0oaqbcmJ3FnbdgxF40g3"
                 , _clientSecret = "RO_574ekzr2WieopIcYzDnfeOCHPAv5Bq6-AbLMm"
                 , _redirectUri = "http://localhost:8080/authorization-code/callback"
                 }
