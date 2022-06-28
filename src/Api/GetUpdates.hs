{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.GetUpdates where

import Control.Monad.State (MonadIO (liftIO), State, runState)
import Data.Aeson (FromJSON)
import qualified Data.Foldable as Seq
import GHC.Conc (readTVarIO)
import GHC.Generics (Generic)
import Servant (Handler)
import Server.Actions (writeAction)
import qualified Server.Actions as Actions
import Server.Context (Context (..))
import Server.Response (Response (Ok))
import Server.Token (Token)
import qualified Server.Token as Token
import ServerState (ServerState, getBotAsBot)
import qualified ServerState.Bot as Bot
import ServerState.Id (Id (..))
import ServerState.Update (Update (Update))
import qualified ServerState.Update as Update

data GetUpdates = GetUpdates
  { offset :: Id,
    limit :: Integer
  }
  deriving (Generic)

instance FromJSON GetUpdates

getRange :: Id -> Int -> [Update] -> [Update]
getRange firstId limit updates = take limit (dropWhile isOldUpdate updates)
  where
    isOldUpdate Update {Update.id} = id < firstId

getUpdates' :: Id -> Integer -> Token -> State ServerState [Update]
getUpdates' offset lim token = do
  bot <- ServerState.getBotAsBot (Token.getId token)
  let upds = case bot of
        Nothing -> []
        Just b -> Seq.toList (Bot.updates b)
  return (getRange offset (fromIntegral lim) upds)

getUpdates :: Context -> GetUpdates -> Handler (Response [Update])
getUpdates
  Context {state, token, actions}
  GetUpdates {offset, limit} = do
    writeAction token actions Actions.GetUpdates
    state' <- liftIO $ readTVarIO state
    let (result, _) = runState (getUpdates' offset limit token) state'
    return (Ok result)
