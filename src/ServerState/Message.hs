{-# LANGUAGE DeriveGeneric #-}

module ServerState.Message (Message (..)) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import ServerState.Id (Id)
import ServerState.Time (Time)

data Message = Message
  { id :: Id,
    from :: Id,
    text :: Text,
    date :: Time
  }
  deriving (Generic)

instance ToJSON Message
