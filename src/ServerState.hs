{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ServerState
    ( ServerState
    , getUser
    , getBots
    , sendMessage
    , initialize
    , getBot
    , Bots
    ) where

import Control.Monad.State (MonadState(get, put), State)
import Data.List (find)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)

import qualified Server.Token as Token

import ServerState.Bot (Bot(Bot, token), addUpdate)
import qualified ServerState.Bot as Bot
import qualified ServerState.Chat as Chat
import ServerState.CompleteMessage (CompleteMessage(CompleteMessage))
import qualified ServerState.CompleteMessage as CompleteMessage
import ServerState.Id (Id(Id))
import ServerState.InitialBot (InitialBot)
import qualified ServerState.InitialBot as InitialBot
import ServerState.PrivateChat (PrivateChat)
import qualified ServerState.PrivateChat as PrivateChat
import ServerState.Time (Time)
import ServerState.User (User(User))
import qualified ServerState.User as User

-- The IDs are sorted in ascending order, as otherwise (1, 2) and (2, 1)
-- would map to different chats. Use `PrivateChat.makeChatId` to generate such
-- an id.
type PrivateChats = Map (Id, Id) PrivateChat

type Bots = Map Id Bot

data ServerState = ServerState
    { users :: [User]
    , privateChats :: PrivateChats
    , bots :: Bots
    }

-- | Initialize a `ServerState` with at least one user and one bot.
initialize
    :: List.NonEmpty.NonEmpty User
    -> List.NonEmpty.NonEmpty ServerState.InitialBot.InitialBot
    -> ServerState
initialize users initialBots = ServerState
    { users = allUsers
    , privateChats = Map.empty
    , bots
    }
  where
    users' = List.NonEmpty.toList users
    initialBots' = List.NonEmpty.toList initialBots
    allUsers = users' ++ botUsers

    botUsers = map makeBotUser initialBots'
    makeBotUser bot = User
        { User.userId = Token.getId (InitialBot.token bot)
        , User.firstName = InitialBot.name bot
        , User.lastName = Nothing
        , User.username = Just (InitialBot.username bot)
        , User.isBot = True
        }

    bots = Map.fromList (map makeBotsEntry initialBots')
    makeBotsEntry initialBot = (botId, bot)
      where
        botId = Token.getId (InitialBot.token initialBot)
        bot = Bot
            { Bot.token = InitialBot.token initialBot
            , Bot.permissions = InitialBot.permissions initialBot
            , Bot.updates = Seq.empty
            , Bot.updateId = Id 0
            }

-- | Return a list of users from the State.
getUsers :: State ServerState [User]
getUsers = do
    ServerState { users } <- get
    return users

-- | Return a user by Id from the State.
getUser :: Id -> State ServerState (Maybe User)
getUser userId = do
    users <- getUsers
    let user = Data.List.find (\x -> User.userId x == userId) users
    return user

-- | Return a list of private chats from the State.
getPrivateChats :: State ServerState PrivateChats
getPrivateChats = do
    ServerState { privateChats } <- get
    return privateChats

-- | Return private chat by Id from the State.
getPrivateChat :: (Id, Id) -> State ServerState (Maybe PrivateChat)
getPrivateChat chatId = Map.lookup chatId <$> getPrivateChats

-- | Add new private chat to the State.
putPrivateChat :: (Id, Id) -> PrivateChat -> State ServerState ()
putPrivateChat chatId chat = do
    state@ServerState { privateChats } <- get
    put (state { privateChats = Map.insert chatId chat privateChats })

-- | Return a map of bots from the state.
getBots :: State ServerState Bots
getBots = do
    ServerState { bots } <- get
    return bots

-- | Get information about a bot given its ID.
getBot :: Id -> State ServerState (Maybe Bot)
getBot botId = do
    bots <- getBots
    let bot = Map.lookup botId bots
    return bot

putUpdatedBot :: Maybe Bot -> State ServerState ()
putUpdatedBot Nothing = return ()
putUpdatedBot (Just bot) = do
    state@ServerState { bots } <- get
    let botId = Token.getId (token bot)
    put (state { bots = Map.insert botId bot bots })

-- | Send new message in private chat.
sendMessage :: Id -> Id -> Time -> Text -> State ServerState CompleteMessage
sendMessage from to date text = do
    fromUser <- getUser from
    toUser <- getUser to

    let chatId = PrivateChat.makeId from to

    chat <- getPrivateChat chatId
    let chat' = fromMaybe PrivateChat.empty chat
    let (message, updatedChat) = PrivateChat.addMessage chat' from date text
    let completeMessage = CompleteMessage
            { CompleteMessage.message = message
              -- TODO: replace `Id`s in the arguments with `User` and `Chat` to
              -- verify that the sender and the chat really exist
            , CompleteMessage.from = fromJust fromUser
            , CompleteMessage.chat = Chat.PrivateChat (fromJust toUser)
            }

    bot <- getBot to
    let updatedBot = addUpdate bot completeMessage

    putUpdatedBot updatedBot
    putPrivateChat chatId updatedChat
    return completeMessage
