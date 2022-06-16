module State (State (State)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence
import Server.Token (Token)
import State.Chat
import State.Id
import State.Message (Message)
import State.Update (Update)
import State.User

data State = State
  { users :: [User],
    -- The IDs are sorted in ascending order, as otherwise (1, 2) and (2, 1)
    -- would map to different chats.
    privateChats :: Map (Id, Id) PrivateChat,
    bots :: Map Id Bot
  }

data Bot = Bot
  { token :: Token,
    updates :: Sequence Update
  }

-- User
-- [Chat] -> Chat Map<(user1Id,user2Id) : [Message]>
-- Map <botid : {token::Token, updatesTohadle :: Queue Update}>

-- data Update = { id :: integer, message :: Message (to be cont.)}

-- getUser :: State -> Id -> Maybe User
-- sendMessage :: State -> Id -> Id -> Text -> State
