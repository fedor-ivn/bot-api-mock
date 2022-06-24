{-# LANGUAGE DeriveGeneric #-}

module ServerState.Update (Update (..)) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Id (Id)

data Update = Update
  { id :: Id,
    -- TODO: should contain not a CompleteMessage but IDs to account for
    -- possible edits of user information
    message :: CompleteMessage
  }
  deriving (Generic)

instance ToJSON Update
