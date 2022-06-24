{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerState.Id (Id (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- | An Id of a User.
newtype Id = Id Int64 deriving (Ord, Eq, Show, Generic, Enum)

instance ToJSON Id

instance FromJSON Id