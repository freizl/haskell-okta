

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Ingredients.Basic
import           Test.Tasty.Runners.AntXML

import qualified Specs.AppOktaHostedLogin as AppOktaHostedLogin
import qualified Specs.AppCustomLogin as AppCustomLogin
import qualified Specs.Config                 as SC
import qualified Specs.Token                  as ST


unitTests :: IO TestTree
unitTests = do
  a1 <- testSpec "AppOktaHostedLogin" AppOktaHostedLogin.spec
  a2 <- testSpec "AppCustomLogin" AppCustomLogin.spec
  c <- testSpec "Config" SC.spec
  t <- testSpec "Token" ST.spec
  return $ testGroup "Smaples-Haskell-Scotty Okta Hosted Login Unit Tests" [c, t, a1, a2]

main :: IO ()
main = unitTests >>= defaultMainWithIngredients [antXMLRunner, consoleTestReporter]
