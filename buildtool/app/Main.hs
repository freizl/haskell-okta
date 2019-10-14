{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Options.Applicative
import qualified Turtle.Prelude         as TT
import qualified Turtle.Shell           as TT


data BuildToolOptions = Lint
                        | Stylish
                        | TestUnit TestUnitOptions
                        | TestE2E
                      deriving (Show)

newtype TestUnitOptions = TestUnitOptions { testReport   :: Bool
                                          } deriving (Show)

sh :: MonadIO io => Text -> io ()
sh li = TT.shells li TT.stdin

pl :: MonadIO io => [IO a] -> io ()
pl = TT.sh . TT.parallel

testunitOptionsP :: Parser BuildToolOptions
testunitOptionsP = fmap TestUnit $ TestUnitOptions
                   <$> switch ( long "create-test-report"
                                <> short 'r'
                                <> help "run unit test and generate checkstyle xml report" )

buildToolOptions :: Parser BuildToolOptions
buildToolOptions = subparser
  (command "lint" (info (pure Lint) (progDesc "run hlint"))
    <> command "stylish" (info (pure Stylish) (progDesc "format code via stylish-haskell"))
    <> command "unit" (info (testunitOptionsP <**> helper) (progDesc "run unit test"))
    <> command "e2e" (info (pure TestE2E) (progDesc "run Okta OAuth test suite"))
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
command_ Lint           = lint
command_ Stylish        = stylish
command_ (TestUnit opt) = testunit opt
command_ TestE2E        = teste2e


lint :: IO ()
lint = pl
  [ sh "stack exec hlint samples/custom-login/app "
  , sh "stack exec hlint samples/okta-hosted-login/app"
  , sh "stack exec hlint samples/okta-samples-unit-test/test"
  , sh "stack exec hlint samples/okta-samples-common/src"
  , sh "stack exec hlint buildtool/app"
  ]

stylish :: IO ()
stylish = pl
  [ sh "stack exec stylish-haskell -- -i samples/custom-login/app/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/okta-hosted-login/app/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/okta-samples-common/src/Okta/Samples/Common/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/okta-samples-common/src/Data/Aeson/Okta/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/okta-samples-common/src/Web/Scotty/Okta/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/okta-samples-unit-test/test/*.hs"
  , sh "stack exec stylish-haskell -- -i samples/okta-samples-unit-test/test/Specs/*.hs"
  , sh "stack exec stylish-haskell -- -i buildtool/app/*.hs"
  ]


testunit :: TestUnitOptions -> IO ()
testunit opt =
  sh $ T.unwords [ "cabal v2-test"
                  , "all"
                  , if testReport opt then "--test-arguments '--xml=./build2/reports/junit/results.xml'" else ""
                  ]

teste2e :: IO ()
teste2e = sh "cd samples/test-e2e && yarn install && yarn test"
