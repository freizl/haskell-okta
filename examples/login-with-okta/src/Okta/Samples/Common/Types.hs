{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Okta.Samples.Common.Types where

import           Crypto.JOSE.JWK
import           Crypto.JWT

import           Control.Lens        hiding (iat, (.=))
import           Data.Aeson.TH
import           Data.Text.Lazy      (Text)
import           Prelude             hiding (exp)

import           Data.Aeson.Okta.Ext

type Code = Text
type State = Text
type Nonce = Text
type Port = Int
type Email = Text

--------------------------------------------------
-- * Okta API
--------------------------------------------------
type AccessToken = Text
type IDToken = Text

data TokenResponse = TokenResponse { _accessToken :: AccessToken
                                   , _tokenType   :: Text
                                   , _expiresIn   :: Int
                                   , _scope       :: Text
                                   , _idToken     :: IDToken
                                   } deriving (Show, Eq)

newtype KeysResponse = KeysResponse { _keys :: [JWK]
                                    } deriving (Show, Eq)

makeLenses ''TokenResponse
makeLenses ''KeysResponse

$(deriveJSON defaultOptions{fieldLabelModifier = dropAndCamelTo2 1} ''KeysResponse)
$(deriveJSON defaultOptions{fieldLabelModifier = dropAndCamelTo2 1} ''TokenResponse)

data UserInfo = UserInfo
  { _userInfoSub           :: Text
  , _userInfoName          :: Text
  , _userInfoGivenName     :: Text
  , _userInfoFamilyName    :: Text
  , _userInfoEmail         :: Text
  , _userInfoEmailVerified :: Bool
  , _userInfoZoneinfo      :: Text
  } deriving (Show, Eq)

makeLenses ''UserInfo
$(deriveJSON defaultOptions{fieldLabelModifier = dropAndCamelTo2 9} ''UserInfo)

type CookieUser = (ClaimsSet, UserInfo, TokenResponse)

--------------------------------------------------
-- * Sample Config
--------------------------------------------------

data Config = Config { _oidc :: OIDC
                     , _port :: Port
                     } deriving (Show, Eq)

data OIDC = OIDC { _oidcScope        :: Text
                 , _oidcIssuer       :: Text
                 , _oidcClientId     :: Text
                 , _oidcClientSecret :: Text
                 , _oidcRedirectUri  :: Text
                 , _oidcTokenAud     :: Maybe StringOrURI  -- for verify Custom AS AccessToken
                 } deriving (Show, Eq)

makeLenses ''Config
makeLenses ''OIDC

$(deriveJSON defaultOptions{fieldLabelModifier = dropAndCamelTo2 1} ''Config)
$(deriveJSON defaultOptions{fieldLabelModifier = dropAndCamelTo2 5} ''OIDC)
-- TODO: consider omit client secret at toJSON instance

--------------------------------------------------
-- * Okta Signin Widget Config
--------------------------------------------------

data SigninWidgetConfig =
    SigninWidgetConfig { _widgetBaseUrl     :: Text
                       , _widgetClientId    :: Text
                       , _widgetRedirectUri :: Text
                       , _widgetLogo        :: Text
                       , _widgetAuthParams  :: WidgetAuthParam
                       }
data WidgetAuthParam =
    WidgetAuthParam { _widgetIssuer       :: Text
                    , _widgetResponseType :: Text
                    , _widgetDisplay      :: Text
                    , _widgetScope        :: [Text]
                    }


makeLenses ''SigninWidgetConfig
makeLenses ''WidgetAuthParam

$(deriveJSON defaultOptions{fieldLabelModifier = widgetFieldLabelModifier} ''WidgetAuthParam)
$(deriveJSON defaultOptions{fieldLabelModifier = widgetFieldLabelModifier} ''SigninWidgetConfig)
