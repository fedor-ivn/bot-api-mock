{-# LANGUAGE DeriveGeneric #-}

module ServerState.Id (Id (..)) where

import Data.Aeson (ToJSON)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- | An Id of a User.
newtype Id = Id Int64 deriving (Ord, Eq, Show, Generic)

instance ToJSON Id