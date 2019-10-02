

{-# LANGUAGE OverloadedStrings #-}

module Specs.AppOktaHostedLogin (
  spec
  ) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.String
import qualified Network.Wai                as WAI
import           Test.Hspec
import           Test.Hspec.Wai

import           AppOktaHostedLogin
import           Okta.Samples.Common.Types

import           Specs.Internal

spec :: Spec
spec =
  loginRedirectS *>
  profileS *>
  logoutS *>
  callbackS *>
  homeS

homeS :: Spec
homeS = with (testApp sampleConfig) $
  describe "GET /" $ do
    it "responds with 200" $
      get "/" `shouldRespondWith` 200
    it "responds with desired html string" $ do
      html <- liftIO overviewPage
      get "/" `shouldRespondWith` fromString html

loginRedirectS :: Spec
loginRedirectS = with (testApp sampleConfig) $
  describe "GET /login" $ do
    it "responds with 302" $
      get "/login" `shouldRespondWith` 302
    it "responds scenarios page when no user session" $
      get "/login"
      `shouldRespondWith`
      302 {matchHeaders = ["Location" <:> BS.pack (
        "https://rain.okta1.com/oauth2/default/v1/authorize" ++
        "?" ++
        "client_id=cid-111&" ++
        "response_type=code&" ++
        "response_mode=query&" ++
        "scope=openid profile email&" ++
        "redirect_uri=http://localhost:9191/authorization-code/callback&" ++
        "state=okta-hosted-login-state-xyz&" ++
        "nonce=okta-hosted-login-nonce-123"
      )]}
    it "redirect to user profile when user session presents" $
      getWithUserCookie "/login"
      `shouldRespondWith`
      302 {matchHeaders = ["Location" <:> "/profile"]}


profileS :: Spec
profileS = with (testApp sampleConfig) $
  describe "GET /profile" $ do
    it "responds with 200" $
      getWithUserCookie "/profile" `shouldRespondWith` 200
    it "responds scenarios page when no user session" $ do
      html <- liftIO profilePage
      getWithUserCookie "/profile" `shouldRespondWith` fromString html
    it "redirect to home page when no user session presents" $
      get "/profile"
      `shouldRespondWith`
      302 {matchHeaders = ["Location" <:> "/"]}

logoutS :: Spec
logoutS = with (testApp sampleConfig) $
  describe "GET /logout" $
    it "redirect to home page by clear user cookie" $
      get "/logout"
      `shouldRespondWith`
      302 { matchHeaders = [ "Location" <:> "/"
                           , "Set-Cookie" <:> "user=; Path=/; Max-Age=-1000000000"
                           ]
          }

callbackS :: Spec
callbackS = with (testApp sampleConfig) $
  describe "GET /authorization-code/callback" $ do
    it "show error when error parameter is found" $
      get "/authorization-code/callback?error=not_assigned"
      `shouldRespondWith`
      401 { matchBody = matchBody_ (BS8.pack "<h1>Error</h1><p>not_assigned :</p><div><a href=\"/\">Back to home</a></div>") }
    it "show error when error and error_description parameters are found" $
      get "/authorization-code/callback?error=not_assigned&error_description=user is not assigned to the app"
      `shouldRespondWith`
      401 { matchBody = matchBody_ (BS8.pack "<h1>Error</h1><p>not_assigned : user is not assigned to the app</p><div><a href=\"/\">Back to home</a></div>") }
    it "show error when no code parameter is received" $
      get "/authorization-code/callback"
      `shouldRespondWith`
      401 { matchBody = matchBody_ (BS8.pack "<h1>Error</h1><p>no code found from callback request</p><div><a href=\"/\">Back to home</a></div>") }
    it "show error when no state parameter is received" $
      get "/authorization-code/callback?code=abc123"
      `shouldRespondWith`
      401 { matchBody = matchBody_ (BS8.pack "<h1>Error</h1><p>no state found from callback request</p><div><a href=\"/\">Back to home</a></div>") }
    it "show error when state parameter does not match the state provided in the authorize request" $
      get "/authorization-code/callback?code=abc123&state=890def"
      `shouldRespondWith`
      401 { matchBody = matchBody_ (BS8.pack "<h1>Error</h1><p>state is not match. state from parameter is: 890def . state from cookie is: okta-hosted-login-state-xyz</p><div><a href=\"/\">Back to home</a></div>")}

matchBody_ :: BS8.ByteString -> MatchBody
matchBody_ expected = MatchBody (\_ actual -> actualExpected "body mismatch:" actual expected)

actualExpected :: String -> BS8.ByteString -> BS8.ByteString -> Maybe String
actualExpected message actual expected =
  if actual == expected
  then Nothing
  else Just $ unlines [ message
                      , "  expected: " ++ BS8.unpack expected
                      , "  but got:  " ++ BS8.unpack actual
                      ]

testApp :: Config -> IO WAI.Application
testApp = waiApp defaultAppOption
