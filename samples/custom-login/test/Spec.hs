

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Ingredients.Basic
import           Test.Tasty.Runners.AntXML

import qualified Specs.App                    as SA
import qualified Specs.Config                 as SC
import qualified Specs.Token                  as ST


unitTests :: IO TestTree
unitTests = do
  c <- testSpec "Config" SC.spec
  t <- testSpec "Token" ST.spec
  a <- testSpec "App" SA.spec
  return $ testGroup "Smaples-Haskell-Scotty Custom Login Unit Tests" [c, t, a]

main :: IO ()
main = unitTests >>= defaultMainWithIngredients [antXMLRunner, consoleTestReporter]
