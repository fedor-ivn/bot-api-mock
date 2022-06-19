module ServerState.PrivateChat
  ( PrivateChat (..),
    makeId,
    nextMessageId,
    addMessage,
    empty,
  )
where

import Data.Dates (DateTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import ServerState.Id (Id (Id))
import ServerState.Message (Message (Message))
import qualified ServerState.Message as Message

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
  where
    (Id lastId) = maybe (Id 0) Message.id (listToMaybe messages)

-- | Add new message to a private chat.
addMessage :: PrivateChat -> Id -> DateTime -> Text -> PrivateChat
addMessage chat@(PrivateChat messages) from date text =
  PrivateChat (message : messages)
  where
    nextId = nextMessageId chat
    message =
      Message
        { Message.id = nextId,
          Message.from = from,
          Message.text = text,
          Message.date = date
        }
