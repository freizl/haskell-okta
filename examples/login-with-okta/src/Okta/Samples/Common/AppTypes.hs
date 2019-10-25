{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Okta.Samples.Common.AppTypes where

import           Control.Lens        hiding (iat, (.=))
import           Data.Aeson.TH
import           Data.Text.Lazy      (Text)
import           Web.Scotty.Trans (ScottyT, ActionT)
import Control.Monad.State (StateT)

import           Okta.Samples.Common.Types
import           Okta.Samples.Common.OIDC

--------------------------------------------------
-- * App server option
--------------------------------------------------

data AppOption = AppOption
  { _appIssuer       :: Text
  , _appClientId     :: Text
  , _appClientSecret :: Text
  , _appRedirectUri  :: Text
  , _appScopes       :: [Text]
  , _appCustomAsId   :: Maybe Text         -- ^ Custom Authorization Server ID
  , _appDebug        :: Bool
  , _appPort         :: Int
  } deriving (Show)

makeLenses ''AppOption

data OktaSampleAppState = OktaSampleAppState
  { _appOption :: AppOption
  , _config :: Config
  , _openidConfig :: OpenIDConfiguration
  }
makeLenses ''OktaSampleAppState

type OktaSampleAppScottyM = ScottyT Text (StateT OktaSampleAppState IO)
type OktaSampleAppActionM = ActionT Text (StateT OktaSampleAppState IO)
