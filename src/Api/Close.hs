{-# LANGUAGE NamedFieldPuns #-}
module Api.Close (close) where

import Servant (Handler)
import Server.Response (Response (Ok))
import Server.Context (Context(..))
import Server.Actions (writeAction, ActionKind (LogOut))

close :: Context -> Handler (Response Bool)
close Context {state, token, actions} = do
  writeAction token actions LogOut
  return (Ok True)
