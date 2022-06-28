module ServerState.Message (Message (..)) where

import Data.Text (Text)
import ServerState.Id (Id)
import ServerState.Time (Time)

data Message = Message
  { messageId :: Id,
    from :: Id,
    text :: Text,
    date :: Time
  }
