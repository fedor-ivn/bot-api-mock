{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerState.User.Id (UserId(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- | A user's ID.
newtype UserId = UserId Int64 deriving (Ord, Eq, Show, Generic, Enum)

instance ToJSON UserId
instance FromJSON UserId
