{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Okta.Samples.Common.OIDC where

import           Control.Lens              hiding (iat, (.=))
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bifunctor
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

import           Okta.Samples.Common.Types
-- TODO: really URI
type URI = Text

-- TODO: add more fields
--
data OpenIDConfiguration = OpenIDConfiguration
  { _issuer                :: Text
  , _authorizationEndpoint :: URI
  , _tokenEndpoint         :: URI
  , _userinfoEndpoint      :: URI
  , _jwksUri               :: URI
  }

makeLenses ''OpenIDConfiguration
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 1} ''OpenIDConfiguration)

wellknownUrl :: Text
wellknownUrl = "/.well-known/openid-configuration"

fetchWellKnown :: Config -> ExceptT Text IO OpenIDConfiguration
fetchWellKnown c = ExceptT $ do
  let uri = (c ^. (oidc . oidcIssuer)) <> wellknownUrl
  putStr "fetch openid config from: " >> print uri
  req <- parseRequest (TL.unpack uri)
  resp <- httpLbs req
  return (handleWellKnownResponse resp)

handleWellKnownResponse :: Response ByteString -> Either Text OpenIDConfiguration
handleWellKnownResponse resp = do
    let rawBody = getResponseBody resp
    let rStatus = getResponseStatus resp
    if rStatus == status200 then
        first (("handleWellKnownResponse decode response failed: " <>) . TL.pack) (eitherDecode rawBody)
    else Left $ "handleWellKnownResponse failed: " <> TL.pack (show rStatus) <> TL.decodeUtf8 rawBody

-- Poor man's solution to check whether issuer is Custom AS or Org AS
isCustomAS :: OpenIDConfiguration -> Bool
isCustomAS c = TL.isInfixOf "/oauth2/" (c ^. issuer)
