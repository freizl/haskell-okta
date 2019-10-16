module Data.Aeson.Okta.Ext where

import           Data.Char

widgetFieldLabelModifier :: String -> String
widgetFieldLabelModifier s =
  let x = drop 7 s
      h = head x
  in toLower h : tail x

