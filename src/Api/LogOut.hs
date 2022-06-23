{-# LANGUAGE NamedFieldPuns #-}

module Api.LogOut (logOut) where

import Servant (Handler)
import Server.Actions (ActionKind (LogOut), writeAction)
import Server.Context (Context (..))
import Server.Response (Response (Ok))

logOut :: Context -> Handler (Response Bool)
logOut Context {state, token, actions} = do
  writeAction token actions LogOut
  return (Ok True)
