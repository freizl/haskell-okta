{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           System.Environment

import           App                       (app)
import           Okta.Samples.Common.Types

main :: IO ()
main = do
  -- NOTES: buffer mode changed to BlockBuffering model
  -- when this application is invoked from by other program
  -- like stack or npm script. hence force to LineBuffering mode
  hSetBuffering stdout LineBuffering
  args <- getArgs
  app $ AppOption $ "--debug" `elem` args

