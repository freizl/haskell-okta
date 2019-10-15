{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Okta.App (runApp, waiApp) where

import qualified Control.Applicative                  as CA
import           Control.Lens                         ((^.))
import           Control.Monad
import           Control.Monad.Except
import           Network.Wai.Handler.Warp             (run)
import           Prelude                              hiding (exp)

import           Data.List
import qualified Network.Wai                          as WAI
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty

import           Okta.Samples.Common.OIDC
import           Okta.Samples.Common.Types
import           Web.Scotty.Okta.Handlers

------------------------------
-- App
------------------------------

runApp :: Config -> (OpenIDConfiguration -> IO WAI.Application) -> IO ()
runApp c app = do
  putStrLn ("Starting Server at http://localhost:" ++ show (c ^. port))
  openidConfiguration <- runExceptT (fetchWellKnown c)
  case openidConfiguration of
    Left e -> ioError $ userError $ "Cannot fetch openid configuration" ++ show e
    Right oc -> app oc >>= run (c ^. port)

waiApp :: AppOption -> ScottyM () -> IO WAI.Application
waiApp opt extraScotty =
  scottyApp $ do
    when (opt ^. appServerDebug) (middleware logStdoutDev)
    middleware $ staticPolicy (mapAssetsDir >-> addBase "public")
    defaultHandler globalErrorHandler
    extraScotty

mapAssetsDir :: Policy
mapAssetsDir = policy removeAssetsPrefix
  where removeAssetsPrefix s = stripPrefix "assets/" s CA.<|> Just s
