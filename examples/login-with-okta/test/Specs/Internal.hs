

{-# LANGUAGE OverloadedStrings #-}

module Specs.Internal where

import           Crypto.JWT
import           Data.Aeson                    (encode)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Data.String
import qualified Data.Text.Lazy                as TL
import qualified Lucid.Base                    as H
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai.Test              (SResponse)
import           Prelude                       hiding (exp)
import           Test.Hspec.Wai

import           Okta.Samples.Common.OIDC
import           Okta.Samples.Common.Templates
import           Okta.Samples.Common.Types
import           Okta.Samples.Scotty.Sessions


overviewPage :: IO String
overviewPage = return . TL.unpack . H.renderText $ homeH_ Nothing

loginCustomPage :: IO String
loginCustomPage = return . TL.unpack . H.renderText $ loginH_ sampleConfig

profilePage :: IO String
profilePage = return . TL.unpack . H.renderText $ profileH_ sampleCookieUser

getWithUserCookie :: ByteString -> WaiSession st SResponse
getWithUserCookie path = getWithCookie path userCookie

getWithCookie :: ByteString -> ByteString -> WaiSession st SResponse
getWithCookie path cookies = request methodGet path [(hCookie, cookies)] ""

userCookie :: ByteString
userCookie = sampleAppCookieUserKey `BS.append` "=" `BS.append` BSL.toStrict (encode sampleCookieUser)

sampleCookieUser :: (ClaimsSet, UserInfo)
sampleCookieUser =
  ( emptyClaimsSet
  , UserInfo
    { _userInfoSub = "aecabeuc012"
    , _userInfoName = "Test UserA"
    , _userInfoGivenName = "Test"
    , _userInfoFamilyName = "UserA"
    , _userInfoEmail = "tu2@test.com"
    , _userInfoEmailVerified = True
    , _userInfoZoneinfo = "Test Zone"
    }
  )


sampleConfig :: Config
sampleConfig = Config o 9191
  where o = OIDC { _oidcScope = "openid profile email"
                 , _oidcIssuer = "https://rain.okta1.com/oauth2/default"
                 , _oidcClientId = "cid-111"
                 , _oidcClientSecret = "cs-222"
                 , _oidcRedirectUri = "http://localhost:9191/authorization-code/callback"
                 , _oidcTokenAud = Nothing
                 }

defaultAppOption :: AppOption
defaultAppOption = AppOption
  { _appIssuer = "https://rain.okta1.com/oauth2/default"
  , _appClientId = "cid-111"
  , _appClientSecret = "cs-222"
  , _appScopes = ["openid", "profile", "email"]
  , _appRedirectUri = "http://localhost:9191/authorization-code/callback"
  , _appCustomAsId = Nothing
  , _appDebug = False
  , _appPort = 9191
  }

sampleOpenIDConfig :: OpenIDConfiguration
sampleOpenIDConfig = OpenIDConfiguration
  { _issuer                = "https://rain.okta1.com"
  , _authorizationEndpoint = "https://rain.okta1.com/oauth2/v1/authorize"
  , _tokenEndpoint         = "https://rain.okta1.com/oauth2/v1/token"
  , _userinfoEndpoint      = "https://rain.okta1.com/oauth2/v1/userinfo"
  , _jwksUri               = "https://rain.okta1.com/oauth2/v1/keys"
  }
