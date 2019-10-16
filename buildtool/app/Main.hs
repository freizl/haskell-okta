{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import           Options.Applicative
import qualified Turtle.Prelude         as TT
import qualified Turtle.Shell           as TT


data BuildToolOptions = Lint
                        | Stylish
                      deriving (Show)


sh :: MonadIO io => Text -> io ()
sh li = TT.shells li TT.stdin

pl :: MonadIO io => [IO a] -> io ()
pl = TT.sh . TT.parallel

buildToolOptions :: Parser BuildToolOptions
buildToolOptions = subparser
  (command "lint" (info (pure Lint) (progDesc "run hlint"))
    <> command "stylish" (info (pure Stylish) (progDesc "format code via stylish-haskell"))
  )


main :: IO ()
main = execParser opts >>= command_
  where
    opts = info (buildToolOptions <**> helper)
           ( fullDesc
             <> progDesc "Build tool for Haskell sample project"
             <> header "Welcome to Haskell sample project"
           )

command_ :: BuildToolOptions -> IO ()
command_ Lint    = lint
command_ Stylish = stylish


lint :: IO ()
lint = pl
  [ sh "stack exec hlint samples/login-with-okta/app"
  , sh "stack exec hlint samples/login-with-okta/src"
  , sh "stack exec hlint samples/login-with-okta/test"
  , sh "stack exec hlint buildtool/app"
  ]

stylish :: IO ()
stylish = pl
  [ sh "stack exec stylish-haskell -- -i samples/login-with-okta/app/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/login-with-okta/src/Okta/Samples/Common/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/login-with-okta/src/Okta/Samples/Scotty/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/login-with-okta/src/Data/Aeson/Okta/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/login-with-okta/test/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/login-with-okta/test/Specs/*.hs"
  , sh "stack exec stylish-haskell -- -i buildtool/app/*.hs"
  ]
