module Web.Scotty.Okta.Utils where
import           Data.Text.Lazy            (Text)
import           Web.Scotty.Internal.Types

paramValue :: Text -> [Param] -> [Text]
paramValue key = fmap snd . filter (hasParam key)

hasParam :: Text -> Param -> Bool
hasParam t = (== t) . fst
