{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState.Message (Message (..)) where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Dates (DateTime)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime, utctDayTime)
import GHC.Generics (Generic)
import ServerState.Id (Id)
import ServerState.Time (Time)
import Prelude hiding (id)

data Message = Message
  { id :: Id,
    from :: Id,
    text :: Text,
    date :: Time
  }
  deriving (Generic)

instance ToJSON Message