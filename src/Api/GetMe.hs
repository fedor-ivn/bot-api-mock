{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.GetMe where

import Control.Monad.State (State, liftIO, runState)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Aeson.Flatten (mergeTo)
import Data.Text
import GHC.Conc (TVar (TVar), readTVarIO)
import Servant (Handler (Handler))
import Server.Context (Context (..))
import Server.Response (Response (..))
import Server.Token (Token)
import ServerState (ServerState, getBot)
import ServerState.Id (Id (..))
import ServerState.User (User (User))

data Me = Me
  { user :: User,
    canJoinGroups :: Bool
  }

instance ToJSON Me where
  toJSON Me {user, canJoinGroups} = mergeTo me (toJSON user)
    where
      me = object ["can_join_groups" .= canJoinGroups]

createMe :: User -> Me
createMe user = Me {user = user, canJoinGroups = False}

getMe' :: Token -> State ServerState (Response Me)
getMe' token = do
  bot <- ServerState.getBot token
  case bot of
    Nothing -> return unathorized
    Just bot -> return (Ok (createMe bot))
  where
    unathorized = Error {description = "Unathorized", parameters = Nothing}

getMe :: Context -> Handler (Response Me)
getMe Context {state, token} = do
  state' <- liftIO $ readTVarIO state
  let (result, _) = runState (getMe' token) state'
  return result