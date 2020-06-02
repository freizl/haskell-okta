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
import           Data.String                (fromString)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Network.HTTP.Conduit       (applyBasicAuth, urlEncodedBody)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Prelude                    hiding (exp)

import           Okta.Samples.Common.JWT
import           Okta.Samples.Common.OIDC
import           Okta.Samples.Common.Types
import           Okta.Samples.Common.Utils

defaultCustomASAud :: StringOrURI
defaultCustomASAud = "api://default"

-- TODO break out verify and fetch user
fetchAuthUser :: Config
              -> OpenIDConfiguration
              -> Code
              -> Nonce
              -> ExceptT Text IO (ClaimsSet, UserInfo, TokenResponse)
fetchAuthUser c openidConfig codeP nonceP = do
  tokenResp <- fetchToken c openidConfig codeP
  liftIO (putStr "ID Token:" >> print (tokenResp ^. idToken))
  idTokenClaims <- decodeIdToken tokenResp >>= verifyJWTToken (setIDTokenAud c) openidConfig nonceP
  -- TODO: no nonce from access token.
  -- when (isCustomAS openidConfig) (void $ decodeAccessToken tokenResp >>= (verifyJWTToken (setAccessTokenAud c) openidConfig nonceP))
  userInfo <- fetchUserInfo openidConfig (tokenResp ^. accessToken)
  return (idTokenClaims, userInfo, tokenResp)

setIDTokenAud :: Config -> Config
setIDTokenAud c = set (oidc . oidcTokenAud) (Just . fromString . TL.unpack $ c ^. (oidc . oidcClientId)) c

setAccessTokenAud :: Config -> Config
setAccessTokenAud = set (oidc . oidcTokenAud) (Just defaultCustomASAud)

fetchUserInfo :: OpenIDConfiguration -> AccessToken -> ExceptT Text IO UserInfo
fetchUserInfo openidConfig atk = ExceptT $ do
  req <- updateUserInfoRequest atk <$> parseRequest (TL.unpack $ openidConfig ^. userinfoEndpoint)
  resp <- httpLbs req
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
              -> OpenIDConfiguration
              -> Nonce
              -> SignedJWT
              -> ExceptT Text IO ClaimsSet
verifyJWTToken c oc nonceP jwtData = do
  jwks <- fetchKeys oc
  verifyJwtData c nonceP jwks jwtData

fetchToken :: Config -> OpenIDConfiguration -> Code -> ExceptT Text IO TokenResponse
fetchToken c oc codeP = ExceptT $ do
  req <- genTokenRequest c oc codeP
  handleTokenResponse <$> httpLbs req

genTokenRequest :: Config -> OpenIDConfiguration -> Code -> IO Request
genTokenRequest c oc codeP = updateTokenRequest c codeP <$> parseRequest (TL.unpack (oc ^. tokenEndpoint))

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
updateTokenRequest c codeP = setB . addHA . setQ . addAuthHeader (c ^. oidc)
  where setB = urlEncodedBody []
        -- addHC = addRequestHeader hConnection "close"     -- NOTES: connection header is required by mock server
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

decodeAccessToken :: TokenResponse -> ExceptT Text IO SignedJWT
decodeAccessToken eitherResp =
  let t = (eitherResp ^. accessToken)
  in
  ExceptT $ return $ first (TL.pack . show) (decodeCompact (TL.encodeUtf8 t) :: Either Error SignedJWT)

fetchKeys :: OpenIDConfiguration -> ExceptT TL.Text IO [JWK]
fetchKeys oc = ExceptT $ do
  req <- genKeysRequest oc
  handleKeysResponse <$> httpLbs req

genKeysRequest :: OpenIDConfiguration -> IO Request
genKeysRequest oc = updateH <$> parseRequest (TL.unpack (oc ^. jwksUri))
  where updateH = addRequestHeader hAccept "application/json"

handleKeysResponse :: Response ByteString -> Either Text [JWK]
handleKeysResponse resp = do
  let rawBody = getResponseBody resp
  let rStatus = getResponseStatus resp
  if rStatus == status200 then
    bimap TL.pack (^. keys) $ eitherDecode rawBody
  else Left $ "handleKeysResponse failed: " <> TL.pack (show rStatus) <> TL.decodeUtf8 rawBody
