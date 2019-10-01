{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import qualified Data.Text as T
import           Data.Aeson.TH
import           Data.Aeson.Types

data Message = Message { msgText :: T.Text }
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''Message)

data MessageResponse = MessageResponse { messages :: [Message] }
$(deriveJSON defaultOptions ''MessageResponse)

data WelcomeMessage = WelcomeMessage { message :: T.Text }
$(deriveJSON defaultOptions ''WelcomeMessage)