module ServerState.Message (Message(..)) where

import ServerState.Id (Id)
import ServerState.Message.Content (Content)
import ServerState.Time (Time)

-- | Information about a message.
data Message = Message
    { messageId :: Id
    , from :: Id
    , content :: Content
    , date :: Time
    }
