{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState.Message (Message (..)) where

import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Dates (DateTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import ServerState.Id (Id)
import Prelude (Show (show))

data Message = Message
  { id :: Id,
    from :: Id,
    text :: Text,
    date :: DateTime
  }
  deriving (Generic)

instance ToJSON Message where
  toJSON Message {id, from, text, date} = object ["id" .= id, "from" .= from, "text" .= text, "date" .= show date]
