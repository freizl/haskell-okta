module Okta.Samples.Common.Utils where

import qualified Data.Aeson         as Aeson
import           Data.ByteString    (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as TL

tlToBS :: TL.Text -> ByteString
tlToBS = T.encodeUtf8 . TL.toStrict


parseValue :: Aeson.FromJSON a => Maybe Aeson.Value -> Maybe a
parseValue Nothing = Nothing
parseValue (Just a) = case Aeson.fromJSON a of
  Aeson.Error _   -> Nothing
  Aeson.Success b -> Just b
