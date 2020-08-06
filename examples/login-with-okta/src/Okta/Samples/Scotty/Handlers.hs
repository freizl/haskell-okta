{-# LANGUAGE OverloadedStrings #-}

module Okta.Samples.Scotty.Handlers where

import           Control.Applicative
import           Control.Lens                 ((^.))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Monad.State          as ST
import           Data.Aeson                   (encode)
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as T
import           Network.HTTP.Types
import           Prelude                      hiding (exp)

import           Data.Maybe
import           Web.Scotty.Internal.Types
import           Web.Scotty.Trans

import           Okta.Samples.Common.AppTypes
import           Okta.Samples.Common.OIDC
import           Okta.Samples.Common.Token
import           Okta.Samples.Common.Types
import           Okta.Samples.Scotty.Sessions
import           Okta.Samples.Scotty.Utils
import           Okta.Samples.Scotty.Views

redirectToProfileM :: OktaSampleAppActionM ()
redirectToProfileM = redirect "/profile"

redirectToHomeM :: OktaSampleAppActionM ()
redirectToHomeM = redirect "/"

errorM :: Text -> OktaSampleAppActionM ()
errorM = throwError . ActionError

globalErrorHandler :: Text -> OktaSampleAppActionM ()
globalErrorHandler t = status status401 >> errorTpl t

homeH :: OktaSampleAppActionM ()
homeH = getCookieUserM >>= homeTpl

loginCustomH :: OktaSampleAppActionM ()
loginCustomH = do
  pas <- params
  let renderTypeP = paramValue "renderType" pas
  let renderType = if null renderTypeP then "code" else head renderTypeP
  withCookieUserM (const redirectToProfileM) (loginCustomTpl renderType)

loginRedirectH :: OktaSampleAppActionM ()
loginRedirectH =
  withCookieUserM (const redirectToProfileM) (loginToOkta generatedState generatedNonce)

generatedState :: Text
generatedState = "okta-hosted-login-state-xyz"

generatedNonce :: Text
generatedNonce = "okta-hosted-login-nonce-123"

loginToOkta :: Text -> Text -> OktaSampleAppActionM ()
loginToOkta astate anonce = do
  pas <- params
  let responseTypeP = paramValue "responseType" pas
  let responseType = if null responseTypeP then "code" else head responseTypeP
  let responseMode = if responseType == "code" then "query" else "fragment"

  (c, openidConfig') <- getConfigs
  let oc = c ^. oidc
  let concatParam (a, b) = T.intercalate "=" [a, b]
  let queryStr = T.intercalate "&" $ map concatParam [ ("client_id", oc ^. oidcClientId)
                                    , ("response_type", responseType)
                                    , ("response_mode", responseMode)
                                    , ("scope", oc ^. oidcScope)
                                    , ("redirect_uri", oc ^. oidcRedirectUri)
                                    , ("state", astate)
                                    , ("nonce", anonce)
                                    , ("prompt", "consent")
                                    ]
  let fullurl = T.concat [ openidConfig' ^. authorizationEndpoint
                          , "?"
                          , queryStr
                          ]
  redirect fullurl


profileH :: OktaSampleAppActionM ()
profileH = withCookieUserM profileTpl redirectToHomeM

-- | delete cookie and 302 to okta to logout user session.
logoutH :: OktaSampleAppActionM ()
logoutH =
  deleteWidgetCookieM
  >> deleteCookieUserM
  >> withCookieUserM logoutOkta (return ())

logoutOkta :: CookieUser -> OktaSampleAppActionM ()
logoutOkta (_, _, tokenResp) = do
  (c, openidConfig') <- getConfigs
  let concatParam (a, b) = T.intercalate "=" [a, b]
  let queryStr = T.intercalate "&"
        $ map concatParam [ ("id_token_hint", tokenResp ^. idToken )
                          -- TODO: init application site localhost:port as option in some data type
                          , ("post_logout_redirect_uri", "http://localhost:" <> (T.pack $ show $ c ^. port))
                          ]
  let fullurl = T.concat [ openidConfig' ^. endSessionEndpoint
                         , "?"
                         , queryStr
                         ]
  redirect fullurl


pkceCodeCallbackH :: OktaSampleAppActionM ()
pkceCodeCallbackH = pkceCodeTpl

webCodeCallbackH :: OktaSampleAppActionM ()
webCodeCallbackH = do
  -- params from cookie which is generated by okta-signin-widget
  stateC <- getCookiesM "okta-oauth-state"
  nonceC <- getCookiesM "okta-oauth-nonce"
  authorizeCallbackH (stateC <|> Just generatedState) (nonceC <|> Just generatedNonce)

authorizeCallbackH :: Maybe Text -- ^ state
          -> Maybe Text -- ^ nonce
          -> OktaSampleAppActionM ()
authorizeCallbackH astate anonce= do
  let stateC = maybeToList astate
  let nonceC = maybeToList anonce

  -- params from callback request query
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  let errorP = paramValue "error" pas
  let errorDescP = paramValue "error_description" pas

  -- validation failure hence error flow
  unless (null errorP) (errorM $ T.unwords $ errorP ++ [":"] ++ errorDescP)
  when (null codeP) (errorM "no code found from callback request")
  when (null stateP) (errorM "no state found from callback request")
  when (null nonceC) (errorM "no nonce found in the cookie")
  when (null stateC) (errorM "no state found in the cookie")
  when (stateP /= stateC) (errorM $ T.unwords $
                           ["state is not match. state from parameter is:" ]
                           ++ stateP
                           ++ [ ". state from cookie is:" ]
                           ++ stateC
                          )
  -- successful flow
  handleAuthCallback codeP nonceC


handleAuthCallback :: [Text] -> [Text] -> OktaSampleAppActionM ()
handleAuthCallback codeP nonceC = do
  (c, openidConfig') <- getConfigs
  r' <- liftIO $ runExceptT $ fetchAuthUser c openidConfig' (head codeP) (head nonceC)
  case r' of
    Right userAndClaimsAndResp -> setCookieUserM (BS.toStrict $ encode userAndClaimsAndResp) >> redirectToProfileM
    Left e -> errorM e

getConfigs :: OktaSampleAppActionM (Config, OpenIDConfiguration)
getConfigs = do
  appState <- lift ST.get
  return (appState ^. config, appState ^. openidConfig)
