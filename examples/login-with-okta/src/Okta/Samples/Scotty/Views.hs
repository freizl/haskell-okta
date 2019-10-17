module Okta.Samples.Scotty.Views where

import           Data.Text.Lazy                (Text)
import qualified Lucid.Base                    as H
import           Web.Scotty

import           Okta.Samples.Common.Templates
import           Okta.Samples.Common.Types

homeTpl :: Maybe CookieUser -> ActionM ()
homeTpl = lucidToHtml . homeH_

loginCustomTpl :: Config -> ActionM ()
loginCustomTpl = lucidToHtml . loginH_

profileTpl :: CookieUser -> ActionM ()
profileTpl = lucidToHtml . profileH_

errorTpl :: Text -> ActionM ()
errorTpl = lucidToHtml . errorH_

lucidToHtml :: H.Html () -> ActionM ()
lucidToHtml = html . H.renderText
