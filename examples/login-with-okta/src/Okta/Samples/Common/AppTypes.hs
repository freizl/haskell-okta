{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Okta.Samples.Common.AppTypes where

import           Control.Lens              hiding (iat, (.=))
import           Control.Monad.State       (StateT)
import           Data.Text.Lazy            (Text)
import           Web.Scotty.Trans          (ActionT, ScottyT)

import           Okta.Samples.Common.OIDC
import           Okta.Samples.Common.Types

--------------------------------------------------
-- * App server option
--------------------------------------------------

data AppOption = AppOption
  { _appIssuer       :: Text
  , _appClientId     :: Text
  , _appClientSecret :: Text
  , _appRedirectUri  :: Maybe Text
  , _appScopes       :: [Text]
  , _appCustomAsId   :: Text         -- ^ Custom Authorization Server ID
  , _appUseOrgAs     :: Bool
  , _appDebug        :: Bool
  , _appUseLocalWidget :: Bool  -- ^ use SignIn Widget from `assets/widget`
  , _appPort         :: Int
  } deriving (Show)

makeLenses ''AppOption

data OktaSampleAppState = OktaSampleAppState
  { _appOption    :: AppOption
  , _config       :: Config
  , _openidConfig :: OpenIDConfiguration
  }
makeLenses ''OktaSampleAppState

type OktaSampleAppScottyM = ScottyT Text (StateT OktaSampleAppState IO)
type OktaSampleAppActionM = ActionT Text (StateT OktaSampleAppState IO)
