module ServerState.Id (Id (..)) where

import Data.Int (Int64)

-- | An Id of a User.
newtype Id = Id Int64 deriving (Ord, Eq, Show)
