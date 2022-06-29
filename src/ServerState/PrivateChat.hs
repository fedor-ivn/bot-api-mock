module ServerState.PrivateChat
    ( PrivateChat(..)
    , makeId
    , nextMessageId
    , addMessage
    , empty
    ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)

import ServerState.Id (Id(Id))
import ServerState.Message (Message(Message))
import qualified ServerState.Message as Message
import ServerState.Time (Time)

-- | A private chat between two users.
newtype PrivateChat = PrivateChat [Message]

-- | An empty private chat.
empty :: PrivateChat
empty = PrivateChat []

-- | Generate chat's id in the way that the least id comes first.
--
-- >>> makeId (Id 2) (Id 1)
-- (Id 1,Id 2)
makeId :: Id -> Id -> (Id, Id)
makeId from to
    | from < to = (from, to)
    | otherwise = (to, from)

-- | Create an Id for a new message in the chat.
nextMessageId :: PrivateChat -> Id
nextMessageId (PrivateChat messages) = Id (lastId + 1)
    where (Id lastId) = maybe (Id 0) Message.messageId (listToMaybe messages)

-- | Add new message to a private chat.
addMessage :: PrivateChat -> Id -> Time -> Text -> (Message, PrivateChat)
addMessage chat@(PrivateChat messages) from date text =
    (message, PrivateChat (message : messages))
  where
    message = Message
        { Message.messageId = nextMessageId chat
        , Message.from = from
        , Message.text = text
        , Message.date = date
        }
