{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.GetUpdates (getUpdates, GetUpdates) where

import Control.Monad.State (MonadIO(liftIO), State, evalState)
import Data.Aeson (FromJSON)
import qualified Data.Foldable as Seq
import Data.Maybe (fromMaybe)
import GHC.Conc (atomically, readTVar)
import GHC.Generics (Generic)
import Servant (Handler)

import Server.Actions (writeAction)
import qualified Server.Actions as Actions
import Server.Context (Context(..))
import Server.Internal (Server(Server))
import qualified Server.Internal as Server
import Server.Response (Response(Ok))

import ServerState (ServerState)
import ServerState.Bot (Bot(Bot))
import qualified ServerState.Bot as Bot
import ServerState.Update (Update(Update))
import qualified ServerState.Update as Update
import ServerState.Update.Id (UpdateId(UpdateId))
import qualified ServerState.User as User

-- | Parameters for the `getUpdates` method.
data GetUpdates = GetUpdates
    { offset :: Maybe UpdateId -- ^ The Id of the first update.
    , limit :: Maybe Int -- ^ Maximum number of updates that can be returned.
    }
    deriving Generic

instance FromJSON GetUpdates

-- | Return range of updates starting from offset.
getRange :: UpdateId -> Int -> [Update] -> [Update]
getRange firstId limit updates = take limit (dropWhile isOldUpdate updates)
    where isOldUpdate Update { Update.updateId } = updateId < firstId

-- | Get proper range of updates from Bot.
getUpdates' :: GetUpdates -> Bot -> State ServerState (Response [Update])
getUpdates' GetUpdates { offset, limit } Bot { Bot.updates } = return
    (Ok (getRange offset' limit' updates'))
  where
    updates' = Seq.toList updates
    limit' = fromMaybe 100 limit
    offset' = fromMaybe (UpdateId 0) offset

-- | Returns a list of incoming updates.
getUpdates :: Context -> GetUpdates -> Handler (Response [Update])
getUpdates Context { bot, botUser, server } parameters = do
    writeAction (User.userId botUser) actions Actions.GetUpdates
    liftIO $ atomically $ do
        state <- readTVar stateVar
        let response = evalState (getUpdates' parameters bot) state
        return response
    where Server { Server.actions, Server.stateVar } = server
