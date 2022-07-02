{-# LANGUAGE NamedFieldPuns #-}

module Api.LogOut (logOut) where

import Servant (Handler)

import Server.Actions (ActionKind(LogOut), writeAction)
import Server.Context (Context(..))
import Server.Internal (Server(Server))
import qualified Server.Internal as Server
import Server.Response (Response(Ok))

import ServerState.User (User(User))
import qualified ServerState.User as User

-- | Log out bot from the cloud Bot API Server before launching the bot locally.
--  Currently implemented as noop.
logOut :: Context -> Handler (Response Bool)
logOut Context { botUser = User { User.userId }, server } = do
    writeAction userId actions LogOut
    return (Ok True)
    where Server { Server.actions } = server
