module Okta.Samples.Scotty.Views where

import           Data.Text.Lazy                (Text)
import qualified Lucid.Base                    as H
import           Web.Scotty.Trans
import qualified Control.Monad.State          as ST
import Control.Monad.State (lift)

import           Okta.Samples.Common.AppTypes
import           Okta.Samples.Common.Templates
import           Okta.Samples.Common.Types

homeTpl :: Maybe CookieUser -> OktaSampleAppActionM ()
homeTpl cu = do
  appState <- lift ST.get
  lucidToHtml (homeH_ appState cu)

loginCustomTpl :: Text -> OktaSampleAppActionM ()
loginCustomTpl renderType = lift ST.get >>= lucidToHtml . loginH_ renderType

profileTpl :: CookieUser -> OktaSampleAppActionM ()
profileTpl = lucidToHtml . profileH_

errorTpl :: Text -> OktaSampleAppActionM ()
errorTpl = lucidToHtml . errorH_

pkceCodeTpl :: OktaSampleAppActionM ()
pkceCodeTpl = lift ST.get >>= lucidToHtml . pkceCodeH_

lucidToHtml :: H.Html () -> OktaSampleAppActionM ()
lucidToHtml = html . H.renderText
