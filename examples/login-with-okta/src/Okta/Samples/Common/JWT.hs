{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Okta.Samples.Common.JWT where

import           Control.Lens              hiding (iat)
import           Control.Lens.TH           (makeClassyPrisms)
import           Control.Monad
import           Control.Monad.Except
import           Crypto.JOSE.Error
import           Crypto.JOSE.JWK
import           Crypto.JOSE.JWS
import           Crypto.JWT
import qualified Data.HashMap.Strict       as Map
import           Data.Maybe
import qualified Data.Text.Lazy            as TL
import           Data.Time
import           Network.URI               (isURI, parseURI)
import           Prelude                   hiding (exp)

import           Okta.Samples.Common.Types
import           Okta.Samples.Common.Utils

data OktaJWTError
  = OktaJWTError JWTError
  | JWTNonceNotMatch
  | JWTNonceNotFound
  | JWTAlgInHeaderNotMatch
  | JWTNoKeyFoundInHeader
  deriving (Eq, Show)

makeClassyPrisms ''OktaJWTError

instance AsError OktaJWTError where
  _Error = _OktaJWTError . _JWSError

instance AsJWTError OktaJWTError where
  _JWTError = _OktaJWTError


maxClockSkew :: NominalDiffTime
maxClockSkew = 300

verifyJwtData :: Config
              -> Nonce
              -> [JWK]
              -> SignedJWT
              -> ExceptT TL.Text IO ClaimsSet
verifyJwtData c nonceP jwks jwt = withExceptT (TL.pack . show) $ do
  -- liftIO $ print c
  -- liftIO $ print jwt
  let keyIdsFromJwtHeader = jwt ^.. signatures ^.. traverse . header . kid . _Just . param
  when (null keyIdsFromJwtHeader) (throwError (review _JWTNoKeyFoundInHeader ()))
  let jwks' = [ k1 | k1 <- jwks, k2 <- keyIdsFromJwtHeader, (k1 ^. jwkKid) == Just k2]
  let firstJWK = head jwks' -- TODO: forgot why pick first one?
  let conf = jwtValidationSettings c
  claim <- verifyClaims conf (JWKSet jwks) jwt :: ExceptT OktaJWTError IO ClaimsSet
  oktaValidateAlg firstJWK jwt
  oktaValidateNonceClaim nonceP claim
  return claim

jwtValidationSettings :: Config -> JWTValidationSettings
jwtValidationSettings c = defaultJWTValidationSettings (audPredicate c)
  & jwtValidationSettingsAllowedSkew .~ maxClockSkew
  & jwtValidationSettingsCheckIssuedAt .~ True
  & jwtValidationSettingsIssuerPredicate .~ issPredicate c
  where audPredicate config' su = Just su == (config' ^. (oidc . oidcTokenAud))
        issPredicate config' iss' = let uri' = TL.unpack (config' ^. (oidc . oidcIssuer))
                                    in
                                      isURI uri' && (iss' ^? uri  == parseURI uri')

oktaValidateNonceClaim
  :: (AsOktaJWTError e, MonadError e m)
  => Nonce
  -> ClaimsSet
  -> m ()
oktaValidateNonceClaim nonceP claims' = do
  let uc = claims' ^. unregisteredClaims
  let nonce' = Map.lookup "nonce" uc
  case parseValue nonce' of
    Nothing -> throwError (review _JWTNonceNotFound ())
    Just a ->
      if a == nonceP
        then pure ()
        else throwError (review _JWTNonceNotMatch ())


oktaValidateAlg :: (AsOktaJWTError e, MonadError e m)
  => JWK
  -> SignedJWT
  -> m ()
oktaValidateAlg k jwt = do
  let algOfJWT = jwt ^.. signatures ^.. traverse . header . alg . param
  let algOfJWK = k ^. jwkAlg
  if null [alg1 | alg1 <- algOfJWT, algOfJWK == Just (JWSAlg alg1)] then
    throwError (review _JWTAlgInHeaderNotMatch ())
    else pure ()
