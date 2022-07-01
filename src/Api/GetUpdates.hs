{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.GetUpdates (getUpdates, GetUpdates) where

import Control.Monad.State (MonadIO (liftIO), State, evalState)
import Data.Aeson (FromJSON)
import qualified Data.Foldable as Seq
import GHC.Conc (atomically, readTVar)
import GHC.Generics (Generic)
import Servant (Handler)
import Server.Actions (writeAction)
import qualified Server.Actions as Actions
import Server.Context (Context (..))
import Server.Response (Response (Ok))
import ServerState (ServerState)
import ServerState.Bot (Bot (Bot))
import qualified ServerState.Bot as Bot
import ServerState.Id (Id (..))
import ServerState.Update (Update (Update))
import qualified ServerState.Update as Update
import qualified ServerState.User as User

data GetUpdates = GetUpdates
  { offset :: Maybe Id,
    limit :: Maybe Int
  }
  deriving (Generic)

instance FromJSON GetUpdates

isOldUpdate :: Id -> Update -> Bool
isOldUpdate id Update {Update.updateId} = updateId < id

getRange :: Maybe Id -> Maybe Int -> [Update] -> [Update]
getRange Nothing Nothing updates = take 100 updates
getRange Nothing (Just limit) updates = take limit updates
getRange (Just firstId) Nothing updates =
  take
    100
    (dropWhile (isOldUpdate firstId) updates)
getRange (Just firstId) (Just limit) updates =
  take
    limit
    (dropWhile (isOldUpdate firstId) updates)

getUpdates' :: GetUpdates -> Bot -> State ServerState (Response [Update])
getUpdates' GetUpdates {offset, limit} Bot {Bot.updates} = do
  return (Ok (getRange offset limit updates'))
  where
    updates' = Seq.toList updates

getUpdates :: Context -> GetUpdates -> Handler (Response [Update])
getUpdates Context {state, bot, botUser, actions} parameters = do
  writeAction (User.userId botUser) actions Actions.GetUpdates
  liftIO $
    atomically $ do
      state' <- readTVar state
      let response = evalState (getUpdates' parameters bot) state'
      return response
