{-# LANGUAGE OverloadedStrings #-}

module AppOktaHostedLogin (app, waiApp) where


import           Data.Text.Lazy            (Text)
import qualified Network.Wai               as WAI
import           Prelude                   hiding (exp)
import           Web.Scotty

import           Okta.Samples.Common.OIDC
import           Okta.Samples.Common.Types
import           Okta.Samples.Common.Utils
import qualified Web.Scotty.Okta.App       as Okta
import           Web.Scotty.Okta.Handlers
import           Web.Scotty.Okta.Sessions


------------------------------
-- App
------------------------------

app :: AppOption -> IO ()
app opt = do
  cf <- readConfigFile
  case cf of
    Left l  -> print l
    Right c -> Okta.runApp c (waiApp opt c)

waiApp :: AppOption -> Config -> OpenIDConfiguration -> IO WAI.Application
waiApp opt c oc = Okta.waiApp opt $ do
  get "/login" $ loginRedirectH c oc
  get "/authorization-code/callback" $ callbackH c oc
  get "/" homeH
  get "/profile" $ profileH c
  get "/logout" logoutH
--------------------------------------------------
-- * Handlers
--------------------------------------------------

loginRedirectH :: Config -> OpenIDConfiguration -> ActionM ()
loginRedirectH c oc = withCookieUserM (const redirectToProfileM) (loginToOkta c oc generatedState generatedNonce)

callbackH :: Config -> OpenIDConfiguration -> ActionM ()
callbackH c oc = authorizeCallbackH c oc (Just generatedState) (Just generatedNonce)

generatedState :: Text
generatedState = "okta-hosted-login-state-xyz"

generatedNonce :: Text
generatedNonce = "okta-hosted-login-nonce-123"
