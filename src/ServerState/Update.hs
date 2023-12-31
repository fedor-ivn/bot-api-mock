{-# LANGUAGE DeriveGeneric #-}

module ServerState.Update (Update(..)) where

import Data.Aeson
    ( Options(fieldLabelModifier)
    , ToJSON(toJSON)
    , camelTo2
    , defaultOptions
    , genericToJSON
    )
import GHC.Generics (Generic)

import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Update.Id (UpdateId)

-- | Information about an update.
data Update = Update
    { updateId :: UpdateId
      -- TODO: should contain not a CompleteMessage but IDs to account for
      -- possible edits of user information
    , message :: CompleteMessage
    }
    deriving Generic

instance ToJSON Update where
    toJSON = genericToJSON options
        where options = defaultOptions { fieldLabelModifier = camelTo2 '_' }
