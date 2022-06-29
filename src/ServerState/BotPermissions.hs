{-# LANGUAGE DeriveGeneric #-}

module ServerState.BotPermissions
    ( BotPermissions(..)
    , defaultPermissions
    ) where

import Data.Aeson
    ( Options(fieldLabelModifier)
    , ToJSON(toJSON)
    , camelTo2
    , defaultOptions
    , genericToJSON
    )
import GHC.Generics (Generic)

-- | A bot's permissions.
data BotPermissions = BotPermissions
    { canJoinGroups :: Bool
    , canReadAllGroupMessages :: Bool
    , supportsInlineQueries :: Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON BotPermissions where
    toJSON = genericToJSON options
        where options = defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | All bot permissions are set to `False`.
defaultPermissions :: BotPermissions
defaultPermissions = BotPermissions
    { canJoinGroups = False
    , canReadAllGroupMessages = False
    , supportsInlineQueries = False
    }
