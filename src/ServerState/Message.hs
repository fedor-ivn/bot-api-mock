module ServerState.Message (Message(..)) where

import ServerState.Message.Content (Content)
import ServerState.Message.Id (MessageId)
import ServerState.Time (Time)
import ServerState.User.Id (UserId)

-- | Information about a message.
data Message = Message
    { messageId :: MessageId
    , from :: UserId
    , content :: Content
    , date :: Time
    }
