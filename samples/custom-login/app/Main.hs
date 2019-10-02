{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment

import           AppCustomLogin                       (app)
import           Okta.Samples.Common.Types

main :: IO ()
main = do
  args <- getArgs
  app $ AppOption $ "--debug" `elem` args
