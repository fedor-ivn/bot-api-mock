{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.GetMe (Me, getMe) where

import Data.Aeson (ToJSON(toJSON))
import Data.Aeson.Flatten (mergeTo)
import Servant (Handler)

import Server.Actions (ActionKind(GetMe), writeAction)
import Server.Context (Context(..))
import Server.Internal (Server(Server))
import qualified Server.Internal as Server
import Server.Response (Response(..))

import ServerState.Bot (Bot(Bot))
import qualified ServerState.Bot as Bot
import ServerState.BotPermissions (BotPermissions)
import ServerState.User (User)
import qualified ServerState.User as User

-- | Information about the bot who called the `getMe` method.
data Me = Me User BotPermissions

instance ToJSON Me where
    toJSON (Me bot permissions) = mergeTo (toJSON bot) (toJSON permissions)

getMe' :: Bot -> User -> Response Me
getMe' Bot { Bot.permissions } user = Ok (Me user permissions)

-- | Return information about the bot who called this method.
getMe :: Context -> Handler (Response Me)
getMe Context { bot, botUser, server } = do
    writeAction (User.userId botUser) actions GetMe
    return (getMe' bot botUser)
    where Server { Server.actions } = server
