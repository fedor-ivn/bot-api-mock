{-# LANGUAGE NamedFieldPuns #-}

module Api.Close (close) where

import Servant (Handler)

import Server.Actions (ActionKind(LogOut), writeAction)
import Server.Context (Context(..))
import Server.Response (Response(Ok))

import ServerState.User (User(User))
import qualified ServerState.User as User

close :: Context -> Handler (Response Bool)
close Context { botUser = User { User.userId }, actions } = do
    writeAction userId actions LogOut
    return (Ok True)
