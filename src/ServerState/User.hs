{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ServerState.User (User(..)) where

import Data.Aeson
    ( Options(fieldLabelModifier, omitNothingFields)
    , ToJSON(toJSON)
    , camelTo2
    , defaultOptions
    , genericToJSON
    )
import Data.Text (Text)
import GHC.Generics (Generic)

import ServerState.Id (Id)

-- | Information about the user.
data User = User
    { userId :: Id
    , username :: Maybe Text
    , firstName :: Text
    , lastName :: Maybe Text
    , isBot :: Bool
    }
    deriving Generic

instance ToJSON User where
    toJSON = genericToJSON options
      where
        options = defaultOptions
            { fieldLabelModifier = \case
                "userId" -> "id"
                other -> camelTo2 '_' other
            , omitNothingFields = True
            }
