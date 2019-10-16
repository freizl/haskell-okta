{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Okta.Samples.Scotty.App (app, waiApp) where

import           Control.Lens                         ((^.))
import           Control.Monad
import           Control.Monad.Except
import qualified Data.Text.Lazy                       as TL
import qualified Network.Wai                          as WAI
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static        hiding ((<|>))
import           Prelude                              hiding (exp)
import           Web.Scotty

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
  runApp c (waiApp opt c)


fromAppOptionToConfig :: AppOption -> Config
fromAppOptionToConfig AppOption{..} =
  let iss = maybe _appIssuer (TL.append (_appIssuer `TL.append` "/oauth2/")) _appCustomASId
  in
    Config (OIDC (TL.unwords _appScopes) iss _appClientId _appClientSecret _appRedirectUri Nothing) _appPort

runApp :: Config -> (OpenIDConfiguration -> IO WAI.Application) -> IO ()
runApp c theApp = do
  putStrLn ("Starting Server at http://localhost:" ++ show (c ^. port))
  openidConfiguration <- runExceptT (fetchWellKnown c)
  case openidConfiguration of
    Left e -> ioError $ userError $ "Cannot fetch openid configuration" ++ show e
    Right oc -> theApp oc >>= run (c ^. port)

createWaiApp :: AppOption -> ScottyM () -> IO WAI.Application
createWaiApp opt extraScotty =
  scottyApp $ do
    when (opt ^. appDebug) (middleware logStdoutDev)
    middleware $ staticPolicy (addBase "assets")
    defaultHandler globalErrorHandler
    extraScotty

waiApp :: AppOption -> Config -> OpenIDConfiguration -> IO WAI.Application
waiApp opt c oc = createWaiApp opt $ do
  get "/" homeH
  get "/login-redirect" $ loginRedirectH c oc
  get "/login-siw" $ loginCustomH c
  get "/authorization-code/callback" $ callbackH c oc
  get "/profile" $ profileH c
  get "/logout" logoutH

--------------------------------------------------
-- * Handlers
--------------------------------------------------
