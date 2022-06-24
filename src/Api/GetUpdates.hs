{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.GetUpdates where

import Control.Monad.State (MonadIO (liftIO), MonadState (get), State, runState)
import Data.Aeson (FromJSON)
import qualified Data.Foldable as Seq
import qualified Data.Map as Map
import GHC.Conc (readTVarIO)
import GHC.Generics (Generic)
import Servant (Handler)
import Server.Actions (ActionKind (GetUpdates), writeAction)
import Server.Context (Context (..))
import Server.Response (Response (Ok))
import qualified Server.Token as Token
import ServerState (Bots, ServerState, bots, getBots)
import ServerState.Bot (Bot)
import qualified ServerState.Bot as Bot
import ServerState.Id (Id (..))
import ServerState.Update (Update)

data GetUpdates = GetUpdates
  { offset :: Id,
    limit :: Integer
  }
  deriving (Generic)

instance FromJSON GetUpdates

getRange :: Id -> Integer -> [a] -> [a]
getRange (Id offset) lim xs =
  take
    (fromIntegral lim)
    (take (length xs - fromIntegral offset + 1) xs)

getUpdates' :: Id -> Integer -> Maybe Bot -> State ServerState [Update]
getUpdates' offset lim bot = do
  let upds = case bot of
        Nothing -> []
        Just b -> Seq.toList (Bot.updates b)
  return (getRange offset lim upds)

getUpdates :: Context -> GetUpdates -> Handler (Response [Update])
getUpdates
  Context {state, token, actions}
  Api.GetUpdates.GetUpdates {offset, limit} = do
    writeAction token actions Server.Actions.GetUpdates
    state' <- liftIO $ readTVarIO state
    let bot = Map.lookup (Token.getId token) (bots state')
    let (result, _) = runState (getUpdates' offset limit bot) state'
    return (Ok result)
