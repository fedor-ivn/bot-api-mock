{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ServerState (ServerState (..), getBot, sendMessage) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Dates (DateTime)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Server.Token (Token)
import qualified Server.Token as Token
import ServerState.Id (Id (Id))
import ServerState.Message (Message (Message))
import qualified ServerState.Message as Message
import ServerState.PrivateChat (PrivateChat (PrivateChat), makeId)
import qualified ServerState.PrivateChat as PrivateChat
import ServerState.Update (Update)
import ServerState.User (User (User))
import qualified ServerState.User as User

-- The IDs are sorted in ascending order, as otherwise (1, 2) and (2, 1)
-- would map to different chats. Use `PrivateChat.makeChatId` to generate such
-- an id.
type PrivateChats = Map (Id, Id) PrivateChat

data ServerState = ServerState
  { users :: [User],
    privateChats :: PrivateChats,
    bots :: Map Token Bot
  }

newtype Bot = Bot
  { updates :: Seq Update
  }

-- | Return a list of users from the State.
getUsers :: State ServerState [User]
getUsers = do
  ServerState {users} <- get
  return users

-- | Return a user by Id from the State.
getUser :: Id -> State ServerState (Maybe User)
getUser id = do
  users <- getUsers
  let user = Data.List.find (\x -> User.id x == id) users
  return user

-- | Return a list of private chats from the State.
getPrivateChats :: State ServerState PrivateChats
getPrivateChats = do
  ServerState {privateChats} <- get
  return privateChats

-- | Return private chat by Id from the State.
getPrivateChat :: (Id, Id) -> State ServerState (Maybe PrivateChat)
getPrivateChat chatId = do
  privateChats <- getPrivateChats
  return (Map.lookup chatId privateChats)

-- | Add new private chat to the State.
putPrivateChat :: (Id, Id) -> PrivateChat -> State ServerState ()
putPrivateChat chatId chat = do
  state@ServerState {privateChats} <- get
  put (state {privateChats = Map.insert chatId chat privateChats})

-- | Return a bot by its token from the State.
getBot :: Token -> State ServerState (Maybe User)
getBot token = do
  users <- getUsers
  case Token.getId token of
    Nothing -> return Nothing
    Just id -> getUser id

-- | Send new message in private chat.
sendMessage :: Id -> Id -> DateTime -> Text -> State ServerState ()
sendMessage from to date text = do
  let chatId = PrivateChat.makeId from to

  chat <- getPrivateChat chatId
  let chat' = fromMaybe PrivateChat.empty chat
  let updatedChat = PrivateChat.addMessage chat' from date text

  putPrivateChat chatId updatedChat
