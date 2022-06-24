{-# LANGUAGE NamedFieldPuns #-}

module Api.GetUpdates where

import Control.Monad.State
import Data.Int (Int64)
import GHC.Conc (readTVarIO)
import Servant (Handler)
import Server.Context (Context (Context, state, token))
import Server.Response (Response)
import Server.Token (Token)
import ServerState (ServerState)
import ServerState.Id
import ServerState.Update (Update)

data GetUpdates = GetUpdates
  { offset :: Id,
    limit :: Integer
  }

getUpdates :: Context -> Handler (Response GetUpdates)
getUpdates Context {Server.Context.state, token} = do
  state' <- liftIO $ readTVarIO state
  return _todo