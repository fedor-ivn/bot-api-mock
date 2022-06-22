{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.GetMe (Me, getMe) where

import Control.Monad.State (State, liftIO, runState)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Flatten (mergeTo)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import GHC.Conc (TVar (TVar), readTVarIO)
import Servant (Handler)
import Server.Context (Context (..))
import Server.Response (Response (..))
import Server.Token (Token)
import ServerState (ServerState)
import qualified ServerState
import ServerState.Bot (Bot (Bot))
import qualified ServerState.Bot as Bot
import ServerState.BotPermissions (BotPermissions)
import ServerState.Id (Id)
import ServerState.User (User (User))
import qualified ServerState.User as User

data Me = Me User BotPermissions

instance ToJSON Me where
  toJSON (Me bot permissions) = mergeTo (toJSON bot) (toJSON permissions)

createMe :: Map Id Bot -> User -> Me
createMe bots user@User {User.id} = Me user permissions
  where
    -- todo: get rid of `fromJust`
    Bot {Bot.permissions} = fromJust (Map.lookup id bots)

getMe' :: Token -> State ServerState (Response Me)
getMe' token = do
  bot <- ServerState.getBot token
  bots <- ServerState.getBots
  case bot of
    Nothing -> return unathorized
    Just bot -> return (Ok (createMe bots bot))
  where
    unathorized = Error {description = "Unathorized", parameters = Nothing}

getMe :: Context -> Handler (Response Me)
getMe Context {state, token} = do
  state' <- liftIO $ readTVarIO state
  let (result, _) = runState (getMe' token) state'
  return result
