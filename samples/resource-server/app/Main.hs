module Main where

import           System.Environment

import           AppResourceServer                       (app)
import           Okta.Samples.Common.Types

main :: IO ()
main = do
  args <- getArgs
  app $ AppOption $ "--debug" `elem` args
