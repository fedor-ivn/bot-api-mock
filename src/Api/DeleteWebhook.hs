{-# LANGUAGE NamedFieldPuns #-}

module Api.DeleteWebhook (deleteWebhook) where

import Servant (Handler)

import Server.Actions (ActionKind(DeleteWebhook), writeAction)
import Server.Context (Context(..))
import Server.Response (Response(Ok))

import ServerState.User (User(User))
import qualified ServerState.User as User

deleteWebhook :: Context -> Handler (Response Bool)
deleteWebhook Context { botUser = User { User.userId }, actions } = do
    writeAction userId actions DeleteWebhook
    return (Ok True)
