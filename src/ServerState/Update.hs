{-# LANGUAGE DeriveGeneric #-}

module ServerState.Update (Update (..)) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import ServerState.Id (Id)
import ServerState.Message (Message)

data Update = Update
  { id :: Id,
    message :: Message
  }
  deriving (Generic)

instance ToJSON Update