module ServerState.PrivateChat
    ( PrivateChat(..)
    , nextMessageId
    , addMessage
    , empty
    ) where

import Data.Maybe (listToMaybe)

import ServerState.Id (Id(Id))
import ServerState.Message (Message(Message))
import qualified ServerState.Message as Message
import ServerState.Message.Content (Content)
import ServerState.Time (Time)
import ServerState.User.Id (UserId)

-- | A private chat between two users.
newtype PrivateChat = PrivateChat [Message]

-- | An empty private chat.
empty :: PrivateChat
empty = PrivateChat []

-- | Create an Id for a new message in the chat.
nextMessageId :: PrivateChat -> Id
nextMessageId (PrivateChat messages) = Id (lastId + 1)
    where (Id lastId) = maybe (Id 0) Message.messageId (listToMaybe messages)

-- | Add new message to a private chat.
addMessage
    :: PrivateChat -> UserId -> Time -> Content -> (Message, PrivateChat)
addMessage chat@(PrivateChat messages) from date content =
    (message, PrivateChat (message : messages))
  where
    message = Message
        { Message.messageId = nextMessageId chat
        , Message.from = from
        , Message.content = content
        , Message.date = date
        }
