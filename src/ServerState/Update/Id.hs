{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerState.Update.Id (UpdateId(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- | An update's ID.
newtype UpdateId = UpdateId Int64 deriving (Ord, Eq, Show, Generic, Enum)

instance ToJSON UpdateId
instance FromJSON UpdateId
