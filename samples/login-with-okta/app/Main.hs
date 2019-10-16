{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup            ((<>))
import           Options.Applicative

import           LoginWithOkta             (app)
import           Okta.Samples.Common.Types

main :: IO ()
main = runAppOptionParser >>= app

runAppOptionParser :: IO AppOption
runAppOptionParser = execParser opts
  where
    opts = info (appOptionParser <**> helper)
      ( fullDesc
        <> progDesc "Login with Okta Sample Application"
        <> header "happy hacking with Okta" )

appOptionParser :: Parser AppOption
appOptionParser = AppOption
      <$> strOption
          ( long "issuer"
          <> short 'i'
          <> help "issuer" )
      <*> strOption
          ( long "client_id"
          <> short 'c'
          <> help "client id" )
      <*> strOption
          ( long "client_secret"
          <> short 'x'
          <> help "client secret" )
      <*> strOption
          ( long "redirect_uri"
          <> short 'r'
          <> help "redirect uri"
          <> value "http://localhost:9191/authorization-code/callback"
          <> showDefault
          )
      <*> (some (strOption ( long "scopes"
                          <> short 's'
                          <> help "scopes. default to '-s openid -s profile -s email'."))
            <|>
            pure ["openid", "profile", "email"]
          )
      <*> optional (strOption (long "custom-as" <> help "(optonal) run sample app using custom authorization server."))
      <*> optional (strOption
                      ( long "token-aud"
                        <> short 'a'
                        <> help "(optional) expected access token aud" ))
      <*> switch
          ( long "debug"
          <> short 'd'
          <> help "enable debug mode" )
      <*> option auto
          ( long "app_port"
          <> short 'p'
          <> help "application server port."
          <> value 9191
          <> showDefault
          )
