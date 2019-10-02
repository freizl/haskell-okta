{-# LANGUAGE OverloadedStrings #-}

module App (app) where

import           Prelude                   hiding (exp)
import           Web.Scotty
import           Control.Monad

import           Okta.Samples.Common.Types
import           Okta.Samples.Common.Utils
import qualified Data.Text as T
import Data.Maybe
import Web.Scotty
import Network.HTTP.Types.Status
import qualified Web.Scotty.Okta.App       as Okta
import           Web.Scotty.Okta.Handlers
import           Web.Scotty.Okta.Sessions

import Types

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
      get "/" resourceServerHomeH
      get "/api/messages" $ messageGetH c
      options "/api/messages" $ messageOptionH

--------------------------------------------------
-- * Handlers
--------------------------------------------------


resourceServerHomeH :: ActionM ()
resourceServerHomeH = json $ WelcomeMessage "Hello!  There's not much to see here :) Please grab one of our front-end samples for use with this sample resource server"

messageOptionH :: ActionM ()
messageOptionH = do
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Headers" "authorization"
  setHeader "Access-Control-Allow-Methods" "GET,HEAD,PUT,PATCH,POST,DELETE"
  setHeader "Connection" "keep-alive"
  setHeader "Vary" "Access-Control-Request-Headers"
  status status204


isAuthH :: ActionM Bool
isAuthH = do
  authHeader <- header "Authorization"
  -- TODO: verify access token
  --when (isNothing authHeader) (return False)
  --return True
  --let accossToken = drop (length "Bearer ") authHeader
  return (isJust authHeader)

unAuthHandler :: ActionM ()
unAuthHandler = status unauthorized401

messageGetH :: Config -> ActionM ()
messageGetH c = do
  isauth <- isAuthH
  setHeader "Access-Control-Allow-Origin" "*"
  when (not isauth) unAuthHandler
  json $ MessageResponse
    [ Message "I am a robot."
    , Message "Hello, world!"
    ]
