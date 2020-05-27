module Okta.Samples.Scotty.Views where

import           Data.Text.Lazy                (Text)
import qualified Lucid.Base                    as H
import           Web.Scotty.Trans

import           Okta.Samples.Common.AppTypes
import           Okta.Samples.Common.Templates
import           Okta.Samples.Common.Types

homeTpl :: Maybe CookieUser -> OktaSampleAppActionM ()
homeTpl = lucidToHtml . homeH_

loginCustomTpl :: Config -> OktaSampleAppActionM ()
loginCustomTpl = lucidToHtml . loginH_

profileTpl :: CookieUser -> OktaSampleAppActionM ()
profileTpl = lucidToHtml . profileH_

errorTpl :: Text -> OktaSampleAppActionM ()
errorTpl = lucidToHtml . errorH_

lucidToHtml :: H.Html () -> OktaSampleAppActionM ()
lucidToHtml = html . H.renderText
