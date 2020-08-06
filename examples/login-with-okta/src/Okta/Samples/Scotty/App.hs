{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Okta.Samples.Scotty.App (app, oktaSampleScottyApp) where

import           Control.Lens                         ((^.))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State                  (evalStateT)
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text.Lazy                       as TL
import qualified Network.Wai                          as WAI
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static        hiding ((<|>))
import           Prelude                              hiding (exp)
import           Web.Scotty.Trans

import           Okta.Samples.Common.AppTypes
import           Okta.Samples.Common.OIDC
import           Okta.Samples.Common.Types
import           Okta.Samples.Scotty.Handlers

------------------------------
-- App
------------------------------

app :: AppOption -> IO ()
app opt = do
  print opt
  let c = fromAppOptionToConfig opt
  runApp c (\oc -> createWaiApp (OktaSampleAppState opt c oc) oktaSampleScottyApp)

fromAppOptionToConfig :: AppOption -> Config
fromAppOptionToConfig AppOption{..} =
  let iss = if _appUseOrgAs then _appIssuer else _appIssuer <> "/oauth2/" <> _appCustomAsId
      defaultRedirectUri = "http://localhost:" <> TL.pack (show _appPort) <> "/authorization-code/callback"
      redirectUri = fromMaybe defaultRedirectUri _appRedirectUri
  in
    Config (OIDC (TL.unwords _appScopes) iss _appClientId _appClientSecret redirectUri Nothing) _appPort

runApp :: Config -> (OpenIDConfiguration -> IO WAI.Application) -> IO ()
runApp c theApp = do
  putStrLn ("Starting Server at http://localhost:" ++ show (c ^. port))
  openidConfiguration <- runExceptT (fetchWellKnown c)
  case openidConfiguration of
    Left e -> ioError $ userError $ "Cannot fetch openid configuration" ++ show e
    Right oc -> theApp oc >>= run (c ^. port)

createWaiApp :: OktaSampleAppState -> OktaSampleAppScottyM () -> IO WAI.Application
createWaiApp appState sampleScottyApp =
  scottyAppT (`evalStateT` appState) $ do
    when (appState ^. (appOption . appDebug)) (middleware logStdoutDev)
    middleware $ staticPolicy (addBase "assets")
    defaultHandler globalErrorHandler
    sampleScottyApp

oktaSampleScottyApp :: OktaSampleAppScottyM ()
oktaSampleScottyApp = do
  get "/" homeH
  get "/login-redirect" loginRedirectH
  get "/login-custom" loginCustomH
  get "/authorization-code/callback" webCodeCallbackH
  get "/authorization-code/pkce" pkceCodeCallbackH
  get "/profile" profileH
  get "/logout" logoutH
