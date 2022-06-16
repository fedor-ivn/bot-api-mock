module State.Id (Id (Id)) where

import Data.Int (Int64)

newtype Id = Id Int64 deriving (Ord, Eq)