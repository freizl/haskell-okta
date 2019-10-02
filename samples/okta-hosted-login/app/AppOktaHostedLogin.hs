{-# LANGUAGE OverloadedStrings #-}

module AppOktaHostedLogin (app, waiApp) where


import           Data.Text.Lazy            (Text)
import           Prelude                   hiding (exp)
import           Web.Scotty

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

waiApp opt c = Okta.waiApp opt $ do
  get "/login" $ loginRedirectH c
  get "/authorization-code/callback" $ callbackH c
  get "/" homeH
  get "/profile" $ profileH c
  get "/logout" logoutH
--------------------------------------------------
-- * Handlers
--------------------------------------------------

loginRedirectH :: Config -> ActionM ()
loginRedirectH c = withCookieUserM (const redirectToProfileM) (loginToOkta c generatedState generatedNonce)

callbackH :: Config -> ActionM ()
callbackH c = authorizeCallbackH c (Just generatedState) (Just generatedNonce)

generatedState :: Text
generatedState = "okta-hosted-login-state-xyz"

generatedNonce :: Text
generatedNonce = "okta-hosted-login-nonce-123"
