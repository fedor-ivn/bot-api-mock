{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module ServerState.Message.Id (MessageId(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- | A message ID.
newtype MessageId = MessageId Int64 deriving (Ord, Eq, Show, Generic, Enum)

instance ToJSON MessageId
instance FromJSON MessageId
