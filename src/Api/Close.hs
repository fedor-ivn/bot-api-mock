{-# LANGUAGE NamedFieldPuns #-}

module Api.Close (close) where

import Servant (Handler)

import Server.Actions (ActionKind(LogOut), writeAction)
import Server.Context (Context(..))
import Server.Response (Response(Ok))

import ServerState.User (User(User))
import qualified ServerState.User as User

-- | This method should be used to close the bot instance
-- before moving between local servers.
--  Currently implemented as noop.
close :: Context -> Handler (Response Bool)
close Context { botUser = User { User.userId }, actions } = do
    writeAction userId actions LogOut
    return (Ok True)
