module Okta.Samples.Common.Utils where

-- import           Control.Lens               ((^.))
import qualified Data.Aeson         as Aeson
import           Data.ByteString    (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as TL

-- import           Okta.Samples.Common.Types

-- configFile :: String
-- configFile = ".samples.config.json"

-- readConfigFile :: IO (Either String Config)
-- readConfigFile = do
--   file <- BS.readFile configFile
--   return $ case Aeson.eitherDecode file of
--     Left e   -> Left e
--     Right sc -> Right (sc ^. oktaSampleConfig)


tlToBS :: TL.Text -> ByteString
tlToBS = T.encodeUtf8 . TL.toStrict


parseValue :: Aeson.FromJSON a => Maybe Aeson.Value -> Maybe a
parseValue Nothing = Nothing
parseValue (Just a) = case Aeson.fromJSON a of
  Aeson.Error _   -> Nothing
  Aeson.Success b -> Just b
