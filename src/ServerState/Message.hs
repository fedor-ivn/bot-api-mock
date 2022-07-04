module ServerState.Message (Message(..)) where

import ServerState.Id (Id)
import ServerState.Message.Content (Content)
import ServerState.Time (Time)
import ServerState.User.Id (UserId)

-- | Information about a message.
data Message = Message
    { messageId :: Id
    , from :: UserId
    , content :: Content
    , date :: Time
    }
