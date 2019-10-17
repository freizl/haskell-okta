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
import           Web.Scotty

import           Okta.Samples.Common.Types


type CookieKey = BS.ByteString

sampleAppCookieUserKey :: CookieKey
sampleAppCookieUserKey = "login-with-okta-sample-cookie-user"

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = T.decodeUtf8 . B.toLazyByteString . renderSetCookie

--------------------------------------------------
-- * cookie ActionM
--------------------------------------------------

setCookieM :: CookieKey  -- ^ cookie name
           -> BS.ByteString  -- ^ cookie value
           -> ActionM ()
setCookieM n v = addHeader "Set-Cookie"
  (renderSetCookie' (def { setCookieName = n
                         , setCookieValue = v
                         , setCookiePath = Just "/"
                         }
                    ))

deleteCookieM :: CookieKey  -- cookie name
              -> ActionM ()
deleteCookieM n = addHeader "Set-Cookie"
  (renderSetCookie' (def { setCookieName = n
                        , setCookieValue = ""
                        , setCookiePath = Just "/"
                        , setCookieMaxAge = Just (-1000000000)
                        }
                    ))

-- CookiesText is alias for [(Text, Text)]
--
getCookiesM' :: ActionM (Maybe CookiesText)
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

getCookiesM :: CookieKey -> ActionM (Maybe Text)
getCookiesM key = fmap (getCookieV . T.decodeUtf8 . BSL.fromStrict $ key) getCookiesM'

--------------------------------------------------
-- * User cookie ActionM
--------------------------------------------------

withCookieUserM :: (CookieUser -> ActionM ()) -> ActionM () -> ActionM ()
withCookieUserM yes no = do
  cu <- getCookieUserM
  case cu of
    Nothing -> no
    Just u  -> yes u

getCookieUserM :: ActionM (Maybe CookieUser)
getCookieUserM = maybe Nothing (decode . T.encodeUtf8) <$> getCookiesM sampleAppCookieUserKey

setCookieUserM :: BS.ByteString -> ActionM ()
setCookieUserM = setCookieM sampleAppCookieUserKey

deleteCookieUserM :: ActionM ()
deleteCookieUserM = deleteCookieM sampleAppCookieUserKey

deleteWidgetCookieM :: ActionM ()
deleteWidgetCookieM =
  mapM_
  deleteCookieM
  ["okta-oauth-nonce", "okta-oauth-redirect-params", "okta-oauth-state"]
