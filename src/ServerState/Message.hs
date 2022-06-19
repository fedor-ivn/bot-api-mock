module ServerState.Message (Message (..)) where

import Data.Dates (DateTime)
import Data.Text (Text)
import ServerState.Id (Id)

data Message = Message
  { id :: Id,
    from :: Id,
    text :: Text,
    date :: DateTime
  }
