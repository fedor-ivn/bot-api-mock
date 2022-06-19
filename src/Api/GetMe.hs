{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.GetMe where

import Control.Monad.State (State, liftIO, runState)
import GHC.Conc (TVar (TVar), readTVarIO)
import Servant (Handler (Handler))
import Server.Context (Context (..))
import Server.Response (Response (..))
import Server.Token (Token)
import ServerState (ServerState, getBot)
import ServerState.Id (Id (..))
import ServerState.User (User (User))

getMe' :: Token -> State ServerState (Response User)
getMe' token = do
  bot <- ServerState.getBot token
  return (maybe unathorized Ok bot)
  where
    unathorized = Error {description = "Unathorized", parameters = Nothing}

getMe :: Context -> Handler (Response User)
getMe Context {state, token} = do
  state' <- liftIO $ readTVarIO state
  let (result, _) = runState (getMe' token) state'
  return result