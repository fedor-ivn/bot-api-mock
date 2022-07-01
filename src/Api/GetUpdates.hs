{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.GetUpdates (getUpdates, GetUpdates) where

import Control.Monad.State (MonadIO(liftIO), State, evalState)
import Data.Aeson (FromJSON)
import qualified Data.Foldable as Seq
import Data.Maybe (fromMaybe)
import Data.Sequence (ViewR((:>), EmptyR), viewr)
import GHC.Conc (atomically, readTVar)
import GHC.Generics (Generic)
import Servant (Handler)
import Server.Actions (writeAction)
import qualified Server.Actions as Actions
import Server.Context (Context(..))
import Server.Response (Response(Ok))
import ServerState (ServerState)
import ServerState.Bot (Bot(Bot))
import qualified ServerState.Bot as Bot
import ServerState.Id (Id(..))
import ServerState.Update (Update(Update))
import qualified ServerState.Update as Update
import qualified ServerState.User as User

data GetUpdates = GetUpdates
    { offset :: Maybe Id
    , limit :: Maybe Int
    }
    deriving Generic

instance FromJSON GetUpdates

isOldUpdate :: Id -> Update -> Bool
isOldUpdate id Update { Update.updateId } = updateId < id

getRange :: Id -> Int -> [Update] -> [Update]
getRange firstId limit updates =
    take limit (dropWhile (isOldUpdate firstId) updates)

getUpdates' :: GetUpdates -> Bot -> State ServerState (Response [Update])
getUpdates' GetUpdates { offset, limit } Bot { Bot.updates } = return
    (Ok (getRange offset' limit' updates'))
  where
    updates' = Seq.toList updates
    limit' = fromMaybe 100 limit
    offset' = fromMaybe (Id 0) offset

getUpdates :: Context -> GetUpdates -> Handler (Response [Update])
getUpdates Context { state, bot, botUser, actions } parameters = do
    writeAction (User.userId botUser) actions Actions.GetUpdates
    liftIO $ atomically $ do
        state' <- readTVar state
        let response = evalState (getUpdates' parameters bot) state'
        return response
