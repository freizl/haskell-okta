{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup               ((<>))
import           Options.Applicative

import           Okta.Samples.Common.AppTypes
import           Okta.Samples.Scotty.App      (app)

main :: IO ()
main = runAppOptionParser >>= app

runAppOptionParser :: IO AppOption
runAppOptionParser = execParser opts
  where
    opts = info (appOptionParser <**> helper)
      ( fullDesc
        <> progDesc "Login with Okta Sample Application. By default it's run against Org AS or Custom AS with the ID from parameter 'custom_as'."
        <> header "happy hacking with Okta" )

appOptionParser :: Parser AppOption
appOptionParser = AppOption
      <$> strOption
          ( long "issuer"
          <> short 'i'
          <> help "issuer. (e.g. https://{yourOktaDomain})" )
      <*> strOption
          ( long "client_id"
          <> short 'c'
          <> help "client id" )
      <*> strOption
          ( long "client_secret"
          <> short 'x'
          <> help "client secret" )
      <*> optional (strOption ( long "redirect_uri"
                                <> short 'r'
                                <> help ("redirect uri. " ++
                                         "default to http://localhost:{port}/authorization-code/callback"
                                        )
                              )
                   )
      <*> (some (strOption ( long "scopes"
                          <> short 's'
                          <> help "scopes. (default: '-s openid -s profile -s email')"))
            <|>
            pure ["openid", "profile", "email"]
          )
      <*> strOption (long "custom_as_id"
                    <> short 'm'
                      <> value "default"
                      <> showDefault
                      <> help "Custom AS ID."
                    )
      <*> switch
          ( long "use_org_as"
          <> short 'o'
          <> help "use Org AS. default to false hence use Custom AS by default" )
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
