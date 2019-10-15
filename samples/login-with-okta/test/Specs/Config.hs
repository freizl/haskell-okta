

{-# LANGUAGE OverloadedStrings #-}

module Specs.Config
  ( spec
  , sampleConfig
  ) where

import           Data.Either
import           Okta.Samples.Common.Utils
import           Test.Hspec

import           Specs.Internal

spec :: Spec
spec = describe "Sample Config File" $
  it "exists the file" $ do
    c <- readConfigFile
    isRight c `shouldBe` True
    length (rights [c]) `shouldBe` 1
    head (rights [c]) `shouldBe` sampleConfig
