{-# LANGUAGE DeriveGeneric #-}

module ServerState.User (User (..)) where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericToJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import ServerState.Id (Id)

data User = User
  { id :: Id,
    username :: Maybe Text,
    firstName :: Text,
    lastName :: Maybe Text,
    isBot :: Bool
  }
  deriving (Generic)

instance ToJSON User where
  toJSON = genericToJSON options
    where
      options =
        defaultOptions
          { fieldLabelModifier = camelTo2 '_',
            omitNothingFields = True
          }
