{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Okta.Samples.Common.Token where

import           Control.Applicative        ((<$>))
import           Control.Lens               hiding (iat)
import           Control.Monad
import           Control.Monad.Except
import           Crypto.JOSE.Compact
import           Crypto.JOSE.Error
import           Crypto.JOSE.JWK
import           Crypto.JWT
import           Data.Aeson                 (eitherDecode)
import           Data.Bifunctor
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Network.HTTP.Conduit       (applyBasicAuth, urlEncodedBody)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Prelude                    hiding (exp)

import           Okta.Samples.Common.JWT
import           Okta.Samples.Common.Types
import           Okta.Samples.Common.URIs
import           Okta.Samples.Common.Utils


fetchAuthUser :: Config
              -> Code
              -> Nonce
              -> ExceptT Text IO UserInfo
fetchAuthUser c codeP nonceP = do
  tokenResp <- fetchToken c codeP
  jwtData <- decodeIdToken tokenResp
  _ <- verifyJWTToken c nonceP jwtData
  fetchUserInfo c (tokenResp ^. accessToken)
  -- TODO: to verify access Token since the demo primarily work with custom AS
  -- which access token is JWT


fetchUserInfo :: Config -> AccessToken -> ExceptT Text IO UserInfo
fetchUserInfo c atk = ExceptT $ do
  req <- updateUserInfoRequest atk <$> parseRequest (TL.unpack $ (c ^. (oidc . oidcIssuer)) <> userinfoUrl)
  resp <- httpLbs req
  -- print (getResponseBody resp)
  return (handleUserInfoResponse resp)

handleUserInfoResponse :: Response ByteString -> Either Text UserInfo
handleUserInfoResponse resp = do
  let rawBody = getResponseBody resp
  let rStatus = getResponseStatus resp
  if rStatus == status200 then
    first (("handleUserInfoResponse decode response failed: " <>) . TL.pack) (eitherDecode rawBody)
  else Left $ "handleUserInfoResponse failed: " <> TL.pack (show rStatus) <> TL.decodeUtf8 rawBody

updateUserInfoRequest :: AccessToken -> Request -> Request
updateUserInfoRequest token = addBearer . addHA
  where addHA = addRequestHeader hAccept "application/json"
        addBearer = addRequestHeader hAuthorization (BS.toStrict $ TL.encodeUtf8 $ "Bearer " `TL.append` token)

verifyJWTToken :: Config
              -> Nonce
              -> SignedJWT
              -> ExceptT Text IO SignedJWT
verifyJWTToken c nonceP jwtData = do
  jwks <- fetchKeys c
  verifyJwtData c nonceP jwks jwtData

fetchToken :: Config -> Code -> ExceptT Text IO TokenResponse
fetchToken c codeP = ExceptT $ do
  req <- genTokenRequest c codeP
  handleTokenResponse <$> httpLbs req

genTokenRequest :: Config -> Code -> IO Request
genTokenRequest c codeP = updateTokenRequest c codeP <$> parseRequest (TL.unpack $ (c ^. (oidc . oidcIssuer)) <> tokenUrl)

handleTokenResponse :: Response ByteString -> Either Text TokenResponse
handleTokenResponse resp = do
  let rawBody = getResponseBody resp
  let rStatus = getResponseStatus resp
  if rStatus == status200 then
    first (("handleTokenResp decode response failed: " <>) . TL.pack) (eitherDecode rawBody)
  else Left $ "handleTokenResp failed: " <> TL.pack (show rStatus) <> TL.decodeUtf8 rawBody


updateTokenRequest :: Config
              -> Code
              -> Request
              -> Request
updateTokenRequest c codeP = setB . addHC . addHA . setQ . addAuthHeader (c ^. oidc)
  where setB = urlEncodedBody []
        addHC = addRequestHeader hConnection "close"     -- NOTES: connection header is required by mock server
        addHA = addRequestHeader hAccept "application/json"
        setQ = setRequestQueryString $ map (second Just) [ ("grant_type", "authorization_code")
                                                         , ("code", BS.toStrict (TL.encodeUtf8 codeP))
                                                         , ("redirect_uri", BS.toStrict (TL.encodeUtf8 (c ^. (oidc . oidcRedirectUri))))
                                                         ]


addAuthHeader :: OIDC -> Request -> Request
addAuthHeader o = applyBasicAuth (tlToBS  $ o ^. oidcClientId) (tlToBS $ o ^. oidcClientSecret)


decodeIdToken :: TokenResponse -> ExceptT Text IO SignedJWT
decodeIdToken eitherResp =
  let t = (eitherResp ^. idToken)
  in
   ExceptT $ return $ first (TL.pack . show) (decodeCompact (TL.encodeUtf8 t) :: Either Error SignedJWT)


fetchKeys :: Config -> ExceptT TL.Text IO [JWK]
fetchKeys c = ExceptT $ do
  req <- genKeysRequest c
  handleKeysResponse <$> httpLbs req

genKeysRequest :: Config -> IO Request
genKeysRequest c = updateH <$> parseRequest (TL.unpack $ (c ^. (oidc . oidcIssuer)) <> keyUrl)
  where updateH = addRequestHeader hAccept "application/json"

handleKeysResponse :: Response ByteString -> Either Text [JWK]
handleKeysResponse resp = do
  let rawBody = getResponseBody resp
  let rStatus = getResponseStatus resp
  if rStatus == status200 then
    bimap TL.pack (^. keys) $ eitherDecode rawBody
  else Left $ "handleKeysResponse failed: " <> TL.pack (show rStatus) <> TL.decodeUtf8 rawBody
