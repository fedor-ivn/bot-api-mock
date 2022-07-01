module ServerState.Time where

import Data.Aeson (ToJSON(toJSON))
import Data.Time.Clock (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

newtype Time = Time UTCTime

instance ToJSON Time where
    toJSON (Time time) = toJSON (sinceEpoch time)
      where
        sinceEpoch :: UTCTime -> Integer -- Convert to Unix time
        sinceEpoch =
            floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
