module Data.Aeson.Okta.Ext where

import           Data.Aeson.Types
import           Data.Char

widgetFieldLabelModifier :: String -> String
widgetFieldLabelModifier s =
  let x = drop 7 s
      h = head x
  in toLower h : tail x

dropAndCamelTo2 :: Int -> String -> String
dropAndCamelTo2 n = camelTo2 '_' . drop n
