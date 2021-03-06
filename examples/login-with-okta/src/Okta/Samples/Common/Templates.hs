{-# LANGUAGE OverloadedStrings #-}

module Okta.Samples.Common.Templates where

import           Control.Lens                 ((^.), (.~), (&))
import qualified Data.Aeson                   as Aeson
import           Data.Aeson.Encode.Pretty     (encodePretty)
import           Data.Semigroup               ((<>))
import qualified Data.Text                    as T
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TL
import qualified Lucid.Base                   as H
import           Lucid.Html5

import           Okta.Samples.Common.AppTypes
import           Okta.Samples.Common.Types

datase_ :: T.Text -> H.Attribute
datase_ = data_ "se"

errorH_ :: Text -> H.Html ()
errorH_ er =
  h1_ "Error"
  <> p_ (H.toHtml er)
  <> div_ (a_ [href_ "/"] "Back to home")

headH_ :: H.Html ()
headH_ = head_ $
  meta_ [charset_ "UTF-8"]
  <> title_ "Sample App - Login with Okta"
  <> link_ [href_ "/css/samples.css", type_ "text/css", rel_ "stylesheet"]
  <> base_ [href_ "/"]

menuH_ :: Maybe CookieUser -> H.Html ()
menuH_ muser =
  let menus = case muser of
                Nothing -> mempty
                Just _ -> li_ [] (a_ [id_ "profile-button", class_ "item", href_ "/profile", datase_ "profile"] "My Profile")
                          <>
                          li_ [] (a_ [id_ "logout-button", class_ "item", href_ "/logout", datase_ "logout"] "Logout")
  in
  div_ [class_ "ui inverted left fixed vertical menu"]
  (
    ul_ []
    (
      li_ [] (a_ [href_ "/", class_ "item", id_ "item-overview"] "Home")
      <>
      menus
    )
  )

homeMessageH_ :: OktaSampleAppState -> Maybe CookieUser -> H.Html ()
homeMessageH_ appState Nothing =
  section_ []
  (
    h3_ "For Web App"
    <>
    form_ [method_ "get", action_ "/login-redirect"]
    (
      button_ [id_ "login-button", class_ "ui primary button", type_ "submit"] "Log In from Okta page (Code)"
    )
    <>
    form_ [method_ "get", action_ "/login-custom"]
    (
      input_ [type_ "hidden", name_ "renderType", value_ "code"]
      <>
      button_ [id_ "login-button", class_ "ui primary button", type_ "submit"] "Log In from customized SignIn Widget (Code)"
    )
    <>
    h3_ "For SPA App"
    <>
    form_ [method_ "get", action_ "/login-custom"]
    (
      input_ [type_ "hidden", name_ "renderType", value_ "implicit"]
      <>
      button_ [id_ "login-button", class_ "ui primary button", type_ "submit"] "Log In from customized SignIn Widget (Implicit)"
    )
    <>
    form_ [method_ "get", action_ "/login-custom"]
    (
      input_ [type_ "hidden", name_ "renderType", value_ "pkce"]
      <>
      button_ [id_ "login-button", class_ "ui primary button", type_ "submit"] "Log In from customized SignIn Widget (PKCE)"
    )
    <>
    h3_ "This is OIDC config"
    <>
    pre_ (H.toHtml $ TL.toStrict $ TL.decodeUtf8 $ encodePretty ((appState ^. config . oidc) & oidcClientSecret .~  "xxxxx"))
    <>
    h3_ "Okta API Docs"
    <>
    ul_
    (
      li_ (a_ [ href_ "https://developer.okta.com/authentication-guide/implementing-authentication/auth-code"] "Authorization Code Flow")
    )
  )

homeMessageH_ _ (Just (_, user, _)) =
  let userName = (user ^. userInfoName)
  in
    p_ (H.toHtml $ "Welcome back, " `TL.append` userName)
    <>
    p_ "You have successfully authenticated against your Okta org."
    <>
    p_
    (
      span_ "Visit the "
      <>
      a_ [href_ "/profile"] "My Profile"
      <>
      span_ " page to view User Info and ID Token."
    )

profileH_ :: CookieUser -> H.Html ()
profileH_ cu@(claims, user, _) = baseH_ (Just cu) $
  h2_ [class_ "ui dividing header", datase_ "profile-doc-header"] "My Profile"
  <>
  h3_ "User Info"
  <>
  p_
    (
      span_ (H.toHtml $ "Hello, " `TL.append` (user ^. userInfoName) `TL.append` ". Below is the information that was read from the ")
      <>
      a_ [href_ "https://developer.okta.com/docs/api/resources/oidc#get-user-information"] "User Info Endpoint"
      <>
      span_ " with your Access Token."
    )
  <>
  pre_ (H.toHtml $ TL.toStrict $ TL.decodeUtf8 $ encodePretty user)
  <>
  h3_ "ID Token Claims"
  <>
  pre_ [] (H.toHtml $ encodePretty claims)

homeH_ :: OktaSampleAppState
       -> Maybe CookieUser
       -> H.Html ()
homeH_ appState muser = baseH_ muser
  (
    h2_ [ class_ "ui dividing header", datase_ "overview-doc-header"] "Login with Okta Examples"
    <>
    homeMessageH_ appState muser
  )

baseH_ :: Maybe CookieUser -> H.Html () -> H.Html ()
baseH_ muser mainContent =
  html_ [lang_ "en"]
  (
    headH_
    <>
    body_ [id_ "samples"]
    (
      menuH_ muser
      <>
      div_ [class_ "ui padded grid relaxed", id_ "content"]
      (
        div_ [id_ "doc", class_ "column eight wide"]
        (
          div_ [class_ "doc-overview"] mainContent
        )
      )
    )
  )

widgetResouresLocal :: H.Html ()
widgetResouresLocal =
  let widgetBaseUri = "/widget"
  in
    script_ [src_ (widgetBaseUri `T.append` "/js/okta-sign-in.js"), type_ "text/javascript"] ("" :: Text)
    <>
    link_ [href_ (widgetBaseUri `T.append` "/css/okta-sign-in.css"), type_ "text/css", rel_ "stylesheet"]


widgetResoures :: H.Html ()
widgetResoures =
  let version = "5.3.3"
      widgetBaseUri = "https://global.oktacdn.com/okta-signin-widget/" `T.append` version
  in
    script_ [src_ (widgetBaseUri `T.append` "/js/okta-sign-in.min.js"), type_ "text/javascript"] ("" :: Text)
    <>
    link_ [href_ (widgetBaseUri `T.append` "/css/okta-sign-in.min.css"), type_ "text/css", rel_ "stylesheet"]

loginH_ :: Text -> OktaSampleAppState -> H.Html ()
loginH_ renderType appState =
    html_ [lang_ "en"]
    (
      head_ [] (if appState ^. (appOption . appUseLocalWidget) then widgetResouresLocal else widgetResoures)
      <>
      body_ []
      (
        div_ [id_ "sign-in-widget"] mempty
        <>
        script_
        ( TL.toStrict $
            TL.decodeUtf8 ("const oktaWidgetConfig = " <> Aeson.encode (oidcToWidgetConfig $ appState ^. config)) <> ";"
            <>
            "\nconst widgetRenderType = '" <> renderType <> "';"
        )
        <>
        script_ [src_ "/js/main.js", type_ "text/javascript"] ("" :: Text)
      )
    )

oidcToWidgetConfig :: Config -> SigninWidgetConfig
oidcToWidgetConfig c =
  let o = c ^. oidc
      authParam = WidgetAuthParam
        (o ^. oidcIssuer)
        (TL.splitOn " " (o ^. oidcScope))
  in
    SigninWidgetConfig (TL.replace "/oauth2/default" "" (o ^. oidcIssuer))
      (o ^. oidcClientId)
      (o ^. oidcRedirectUri)
      "http://logofaves.com/wp-content/uploads/2016/07/style_m.jpg?9cf02b"
      authParam

implicitCallbackH_ :: OktaSampleAppState -> H.Html ()
implicitCallbackH_ appState =
  html_ [lang_ "en"]
  (
    head_ [] (if appState ^. (appOption . appUseLocalWidget) then widgetResouresLocal else widgetResoures)
    <>
    body_ []
    (
      h2_ "Finishing implicit flow ..."
      <>
      script_
      ( TL.toStrict $
        TL.decodeUtf8 ("const oktaWidgetConfig = " <> Aeson.encode (oidcToWidgetConfig $ appState ^. config)) <> ";"
        <>
        "\nconst widgetRenderType = '" <> "implicit" <> "';"
      )
      <>
        script_ [src_ "/js/main.js", type_ "text/javascript"] ("" :: Text)
    )
  )
