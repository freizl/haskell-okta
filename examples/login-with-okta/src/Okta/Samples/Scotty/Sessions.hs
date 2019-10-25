{-# LANGUAGE OverloadedStrings #-}

module Okta.Samples.Scotty.Sessions where

import           Control.Applicative       ((<$>))
import           Data.Aeson                (decode)
import qualified Data.Binary.Builder       as B
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.Encoding   as T
import           Web.Cookie
import           Web.Scotty.Trans

import           Okta.Samples.Common.Types
import           Okta.Samples.Common.AppTypes


type CookieKey = BS.ByteString

sampleAppCookieUserKey :: CookieKey
sampleAppCookieUserKey = "login-with-okta-sample-cookie-user"

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = T.decodeUtf8 . B.toLazyByteString . renderSetCookie

--------------------------------------------------
-- * cookie OktaSampleAppActionM
--------------------------------------------------

setCookieM :: CookieKey  -- ^ cookie name
           -> BS.ByteString  -- ^ cookie value
           -> OktaSampleAppActionM ()
setCookieM n v = addHeader "Set-Cookie"
  (renderSetCookie' (def { setCookieName = n
                         , setCookieValue = v
                         , setCookiePath = Just "/"
                         }
                    ))

deleteCookieM :: CookieKey  -- cookie name
              -> OktaSampleAppActionM ()
deleteCookieM n = addHeader "Set-Cookie"
  (renderSetCookie' (def { setCookieName = n
                        , setCookieValue = ""
                        , setCookiePath = Just "/"
                        , setCookieMaxAge = Just (-1000000000)
                        }
                    ))

-- CookiesText is alias for [(Text, Text)]
--
getCookiesM' :: OktaSampleAppActionM (Maybe CookiesText)
getCookiesM' =
    fmap (parseCookiesText . lazyToStrict . T.encodeUtf8) <$> header "Cookie"
    where
      lazyToStrict = BS.concat . BSL.toChunks

getCookieV :: Text -> Maybe CookiesText -> Maybe Text
getCookieV _ Nothing = Nothing
getCookieV _ (Just []) = Nothing
getCookieV key (Just ((a, b): xs))
  | a == T.toStrict key = Just (T.fromStrict b)
  | otherwise = getCookieV key (Just xs)

getCookiesM :: CookieKey -> OktaSampleAppActionM (Maybe Text)
getCookiesM key = fmap (getCookieV . T.decodeUtf8 . BSL.fromStrict $ key) getCookiesM'

--------------------------------------------------
-- * User cookie OktaSampleAppActionM
--------------------------------------------------

withCookieUserM :: (CookieUser -> OktaSampleAppActionM ()) -> OktaSampleAppActionM () -> OktaSampleAppActionM ()
withCookieUserM yes no = getCookieUserM >>= maybe no yes

getCookieUserM :: OktaSampleAppActionM (Maybe CookieUser)
getCookieUserM = maybe Nothing (decode . T.encodeUtf8) <$> getCookiesM sampleAppCookieUserKey

setCookieUserM :: BS.ByteString -> OktaSampleAppActionM ()
setCookieUserM = setCookieM sampleAppCookieUserKey

deleteCookieUserM :: OktaSampleAppActionM ()
deleteCookieUserM = deleteCookieM sampleAppCookieUserKey

deleteWidgetCookieM :: OktaSampleAppActionM ()
deleteWidgetCookieM =
  mapM_
  deleteCookieM
  ["okta-oauth-nonce", "okta-oauth-redirect-params", "okta-oauth-state"]
