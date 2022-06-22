module ServerState.Time where

import Data.Aeson (ToJSON (toJSON))
import Data.Time.Clock (UTCTime (utctDayTime))

newtype Time = Time UTCTime

instance ToJSON Time where
  toJSON (Time time) = toJSON (floor $ utctDayTime time :: Integer)