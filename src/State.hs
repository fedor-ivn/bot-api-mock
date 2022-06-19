{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module State (State (..), getBot, sendMessage) where

import qualified Control.Monad.State as StateM
import Data.Dates (DateTime)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Server.Token (Token)
import qualified Server.Token as Token
import State.Chat (PrivateChat (PrivateChat), makeId)
import qualified State.Chat as Chat
import State.Id (Id (Id))
import State.Message (Message (Message))
import qualified State.Message as Message
import State.Update (Update)
import State.User (User (User))
import qualified State.User as User

-- The IDs are sorted in ascending order, as otherwise (1, 2) and (2, 1)
-- would map to different chats. Use `PrivateChat.makeChatId` to generate such
-- an id.
type PrivateChats = Map (Id, Id) PrivateChat

data State = State
  { users :: [User],
    privateChats :: PrivateChats,
    bots :: Map Token Bot
  }

newtype Bot = Bot
  { updates :: Seq Update
  }

-- | Return a list of users from the State.
getUsers :: StateM.State State [User]
getUsers = do
  State {users} <- StateM.get
  return users

-- | Return a user by Id from the State.
getUser :: Id -> StateM.State State (Maybe User)
getUser id = do
  users <- getUsers
  let user = Data.List.find (\x -> User.id x == id) users
  return user

-- | Return a list of private chats from the State.
getPrivateChats :: StateM.State State PrivateChats
getPrivateChats = do
  State {privateChats} <- StateM.get
  return privateChats

-- | Return private chat by Id from the State.
getPrivateChat :: (Id, Id) -> StateM.State State (Maybe PrivateChat)
getPrivateChat chatId = do
  privateChats <- getPrivateChats
  return (Map.lookup chatId privateChats)

-- | Add new private chat to the State.
putPrivateChat :: (Id, Id) -> PrivateChat -> StateM.State State ()
putPrivateChat chatId chat = do
  state@State {privateChats} <- StateM.get
  StateM.put (state {privateChats = Map.insert chatId chat privateChats})

-- | Return a bot by its token from the State.
getBot :: Token -> StateM.State State (Maybe User)
getBot token = do
  users <- getUsers
  case Token.getId token of
    Nothing -> return Nothing
    Just id -> getUser id

-- | Send new message in private chat.
sendMessage :: Id -> Id -> DateTime -> Text -> StateM.State State ()
sendMessage from to date text = do
  let chatId = Chat.makeId from to

  chat <- getPrivateChat chatId
  let chat' = fromMaybe Chat.empty chat
  let updatedChat = Chat.addMessage chat' from date text

  putPrivateChat chatId updatedChat
