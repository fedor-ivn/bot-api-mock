{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ServerState
    ( ServerState
    , initialize
    , getUser
    , getBot
    , getPrivateChat
    , sendMessage
    ) where

import Control.Monad.State (State)
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)

import qualified Server.Token as Token

import ServerState.Bot (Bot(Bot), addUpdate)
import qualified ServerState.Bot as Bot
import qualified ServerState.Chat as Chat
import ServerState.CompleteMessage (CompleteMessage(CompleteMessage))
import qualified ServerState.CompleteMessage as CompleteMessage
import ServerState.InitialBot (InitialBot)
import qualified ServerState.InitialBot as InitialBot
import ServerState.Internal
    (ServerState(..), getBot, getPrivateChat, getUser, putBot, putPrivateChat)
import ServerState.Message.Content (makeMessageContent)
import qualified ServerState.PrivateChat as PrivateChat
import qualified ServerState.PrivateChat.Id as PrivateChatId
import ServerState.Time (Time)
import ServerState.Update.Id (UpdateId(UpdateId))
import ServerState.User (User(User))
import qualified ServerState.User as User
import ServerState.User.Id (UserId)

-- | Initialize a `ServerState` with at least one user and one bot.
initialize
    :: List.NonEmpty.NonEmpty User
    -> List.NonEmpty.NonEmpty InitialBot
    -> ServerState
initialize initialUsers initialBots = ServerState
    { users
    , privateChats = Map.empty
    , bots
    }
  where
    initialUsers' = List.NonEmpty.toList initialUsers
    initialBots' = List.NonEmpty.toList initialBots

    users' = initialUsers' ++ botUsers
    users = Map.fromList (map makeUsersEntry users')
    makeUsersEntry user@User { User.userId } = (userId, user)

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
            , Bot.nextUpdateId = UpdateId 0
            }

-- | Send new message in private chat.
sendMessage
    :: UserId -> UserId -> Time -> Text -> State ServerState CompleteMessage
sendMessage from to date text = do
    fromUser <- getUser from
    toUser <- getUser to

    let chatId = PrivateChatId.make from to

    chat <- getPrivateChat chatId
    let chat' = fromMaybe PrivateChat.empty chat
    let content = makeMessageContent text
    let (message, updatedChat) = PrivateChat.addMessage chat' from date content
    let completeMessage = CompleteMessage
            { CompleteMessage.message = message
              -- TODO: replace `Id`s in the arguments with `User` and `Chat` to
              -- verify that the sender and the chat really exist
            , CompleteMessage.from = fromJust fromUser
            , CompleteMessage.chat = Chat.PrivateChat (fromJust toUser)
            }

    getBot to >>= \case
        Nothing -> return ()
        Just bot -> putBot (addUpdate completeMessage bot)

    putPrivateChat chatId updatedChat
    return completeMessage
