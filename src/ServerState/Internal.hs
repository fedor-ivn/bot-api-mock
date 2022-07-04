module ServerState.Internal (ServerState(..), PrivateChats, Bots) where

import Data.Map (Map)

import ServerState.Bot (Bot)
import ServerState.PrivateChat (PrivateChat)
import ServerState.PrivateChat.Id (PrivateChatId)
import ServerState.User (User)
import ServerState.User.Id (UserId)

type PrivateChats = Map PrivateChatId PrivateChat
type Bots = Map UserId Bot

-- | The state of the mock Bot API. It stores all of the users, bots and chats.
data ServerState = ServerState
    { users :: [User]
    , privateChats :: PrivateChats
    , bots :: Bots
    }
