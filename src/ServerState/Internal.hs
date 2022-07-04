{-# LANGUAGE NamedFieldPuns #-}

module ServerState.Internal
    ( ServerState(..)
    , getUser
    , getBot
    , putBot
    , getPrivateChat
    , putPrivateChat
    ) where

import Control.Monad.State (MonadState(get, put), State)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Server.Token as Token
import ServerState.Bot (Bot(Bot))
import qualified ServerState.Bot as Bot
import ServerState.PrivateChat (PrivateChat)
import ServerState.PrivateChat.Id (PrivateChatId)
import ServerState.User (User)
import ServerState.User.Id (UserId)

type Users = Map UserId User
type PrivateChats = Map PrivateChatId PrivateChat
type Bots = Map UserId Bot

-- | The state of the mock Bot API. It stores all of the users, bots and chats.
data ServerState = ServerState
    { users :: Users
    , privateChats :: PrivateChats
    , bots :: Bots
    }

-- | Get a user by its ID.
getUser :: UserId -> State ServerState (Maybe User)
getUser userId = do
    ServerState { users } <- get
    let user = Map.lookup userId users
    return user

-- | Get a bot by its ID.
getBot :: UserId -> State ServerState (Maybe Bot)
getBot botId = do
    ServerState { bots } <- get
    let bot = Map.lookup botId bots
    return bot

-- | Update a bot or insert a new one.
putBot :: Bot -> State ServerState ()
putBot bot@Bot { Bot.token } = do
    state@ServerState { bots } <- get

    let botId = Token.getId token
    let bots' = Map.insert botId bot bots
    put (state { bots = bots' })

-- | Get a private chat by its ID.
getPrivateChat :: PrivateChatId -> State ServerState (Maybe PrivateChat)
getPrivateChat chatId = do
    ServerState { privateChats } <- get
    let privateChat = Map.lookup chatId privateChats
    return privateChat

-- | Update a private chat or insert a new one.
putPrivateChat :: PrivateChatId -> PrivateChat -> State ServerState ()
putPrivateChat chatId chat = do
    state@ServerState { privateChats } <- get
    let privateChats' = Map.insert chatId chat privateChats
    put (state { privateChats = privateChats' })
