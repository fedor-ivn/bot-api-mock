{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.GetMe where

import Control.Monad.State (MonadState (get, put), State, liftIO, runState)
import Data.Aeson (Options (fieldLabelModifier, omitNothingFields), ToJSON (toEncoding, toJSON), defaultOptions, genericParseJSON, genericToEncoding, object, pairs, (.=))
import Data.Aeson.Flatten (mergeTo)
import Data.Map (Map, lookup)
import Data.Maybe (fromJust)
import Data.Text ()
import GHC.Conc (TVar (TVar), readTVarIO)
import GHC.Generics (Generic)
import Servant (Handler (Handler))
import Server.Context (Context (..))
import Server.Response (Response (..))
import Server.Token (Token)
import ServerState (Bot (Bot, permissions), ServerState (ServerState, bots), getBot)
import ServerState.BotPermissions (BotPermissions)
import ServerState.Id (Id (..))
import ServerState.User (User (User, id))

data Me = Me User BotPermissions
  deriving (Generic)

instance ToJSON Me where
  toJSON (Me bot permissions) = mergeTo (toJSON bot) (toJSON permissions)

getBotPermissions :: Id -> Map Id Bot -> BotPermissions
getBotPermissions id botMap =
  permissions (fromJust (Data.Map.lookup id botMap))

createMe :: Map Id Bot -> User -> Me
createMe map user =
  Me user (getBotPermissions (ServerState.User.id user) map)

getMe' :: Token -> State ServerState (Response Me)
getMe' token = do
  bot <- ServerState.getBot token
  ServerState {bots} <- get
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