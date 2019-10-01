{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Okta.Handlers where

import           Control.Lens               ((^.))
import           Control.Monad
import           Control.Monad.Except
-- import           Control.Monad.Error.Class
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import           Network.HTTP.Types
import           Prelude                    hiding (exp)

import           Data.Maybe
import           Web.Scotty
import           Web.Scotty.Internal.Types

import           Okta.Samples.Common.Token
import           Okta.Samples.Common.Types
import           Okta.Samples.Common.URIs
import           Web.Scotty.Okta.Sessions
import           Web.Scotty.Okta.Utils
import           Web.Scotty.Okta.Views

redirectToProfileM :: ActionM ()
redirectToProfileM = redirect "/profile"

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

errorM :: Text -> ActionM ()
errorM = throwError . ActionError

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status401 >> html t

homeH :: ActionM ()
homeH = getCookieUserM >>= homeTpl

loginCustomH :: Config -> ActionM ()
loginCustomH c = withCookieUserM (const redirectToProfileM) (loginCustomTpl c)

loginToOkta :: Config -> Text -> Text -> ActionM ()
loginToOkta c astate anonce =
  let oc = c ^. oidc
      concatParam (a, b) = T.intercalate "=" [a, b]
      queryStr = T.intercalate "&" $ map concatParam [ ("client_id", oc ^. oidcClientId)
                                    , ("response_type", "code")
                                    , ("response_mode", "query")
                                    , ("scope", oc ^. oidcScope)
                                    , ("redirect_uri", oc ^. oidcRedirectUri)
                                    , ("state", astate)
                                    , ("nonce", anonce)
                                    ]
      fullurl = T.concat [ c ^. oidc . oidcIssuer
                          , authorizeUrl
                          , "?"
                          , queryStr
                          ]
  in
    redirect fullurl


profileH :: Config -> ActionM ()
profileH _ = withCookieUserM profileTpl redirectToHomeM

logoutH :: ActionM ()
logoutH = deleteCookieUserM >> redirectToHomeM

authorizeCallbackH :: Config
          -> Maybe Text -- ^ state
          -> Maybe Text -- ^ nonce
          -> ActionM ()
authorizeCallbackH c astate anonce= do
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
  when (stateP /= stateC) (errorM $
                           T.unlines $
                           ["state is not match: "] ++
                           [T.unwords $ "state parameter:": stateP] ++
                           [T.unwords $ "state cookie:": stateC]
                          )
  -- successful flow
  handleAuthCallback c codeP nonceC


handleAuthCallback :: Config -> [Text] -> [Text] -> ActionM ()
handleAuthCallback c codeP nonceC = do
  r' <- liftIO $ runExceptT $ fetchAuthUser c (head codeP) (head nonceC)
  case r' of
    Right userInfo -> setCookieUserM (BS.toStrict $ encode userInfo) >> redirectToProfileM
    Left e -> errorM e
