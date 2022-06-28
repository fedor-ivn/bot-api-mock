{-# LANGUAGE NamedFieldPuns #-}

module Api.Close (close) where

import Servant (Handler)
import Server.Actions (ActionKind (LogOut), writeAction)
import Server.Context (Context (..))
import Server.Response (Response (Ok))

close :: Context -> Handler (Response Bool)
close Context {token, actions} = do
  writeAction token actions LogOut
  return (Ok True)
