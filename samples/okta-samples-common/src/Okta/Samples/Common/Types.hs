{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Okta.Samples.Common.Types where

import           Crypto.JOSE.JWK

import           Control.Lens        hiding (iat, (.=))
import           Data.Aeson          (FromJSON, parseJSON)
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text.Lazy      (Text)
import           Prelude             hiding (exp)

import           Data.Aeson.Okta.Ext

type Code = Text
type State = Text
type Nonce = Text
type Port = Int
type Email = Text

--------------------------------------------------
-- * App server option
--------------------------------------------------

newtype AppOption = AppOption { _appServerDebug :: Bool }
                  deriving (Show, Eq)

defaultAppOption :: AppOption
defaultAppOption = AppOption False

makeLenses ''AppOption

--------------------------------------------------
-- * Okta API
--------------------------------------------------
type AccessToken = Text

data TokenResponse = TokenResponse { _accessToken :: AccessToken
                                   , _tokenType   :: String
                                   , _expiresIn   :: Int
                                   , _scope       :: String
                                   , _idToken     :: Text
                                   } deriving (Show, Eq)

newtype KeysResponse = KeysResponse { _keys :: [JWK]
                                    } deriving (Show, Eq)

makeLenses ''TokenResponse
makeLenses ''KeysResponse

instance FromJSON KeysResponse where
    parseJSON (Object v) = KeysResponse <$>
                           v .: "keys"
    parseJSON _          = mempty

instance FromJSON TokenResponse where
    parseJSON (Object v) = TokenResponse
                           <$> v .: "access_token"
                           <*> v .: "token_type"
                           <*> v .: "expires_in"
                           <*> v .: "scope"
                           <*> v .: "id_token"
    parseJSON _          = mempty

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
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 9} ''UserInfo)

type CookieUser = UserInfo

--------------------------------------------------
-- * Sample Config
--------------------------------------------------

newtype OktaSample = OktaSample { _oktaSampleConfig :: Config
                                } deriving (Show, Eq)

data Config = Config { _oidc :: OIDC
                     , _port :: Port
                     } deriving (Show, Eq)

data OIDC = OIDC { _oidcScope        :: Text
                 , _oidcIssuer       :: Text
                 , _oidcClientId     :: Text
                 , _oidcClientSecret :: Text
                 , _oidcRedirectUri  :: Text
                 } deriving (Show, Eq)

makeLenses ''OktaSample
makeLenses ''Config
makeLenses ''OIDC


instance FromJSON OktaSample where
    parseJSON (Object v) = OktaSample <$>
                           v .: "webServer"
    parseJSON _          = mempty


instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "oidc" <*>
                           v .: "port"
    parseJSON _          = mempty

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 5} ''OIDC)

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
