{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.SendMessage where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, evalState, runState)
import Data.Aeson (FromJSON)
import Data.Dates (getCurrentDateTime)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Conc (atomically, readTVar, writeTVar)
import GHC.Generics (Generic)
import Servant (Handler)
import Server.Context (Context (Context, state, token))
import Server.Response (Response (Ok))
import Server.Token (Token (Token))
import qualified Server.Token as Token
import ServerState (ServerState (ServerState), sendMessage)
import qualified ServerState as ServerSate
import ServerState.Id (Id (..))
import ServerState.Message (Message)

data SendMessage = SendMessage {chatId :: Id, text :: Text} deriving (Generic)

instance FromJSON SendMessage

getFrom :: Token -> Id
getFrom token = fromMaybe (Id 0) (Token.getId token)

sendMessage :: Context -> SendMessage -> Handler (Response Message)
sendMessage Context {state, token} SendMessage {chatId, text} = do
  date <- liftIO getCurrentDateTime
  liftIO $
    atomically $ do
      state' <- readTVar state
      let from = getFrom token
      let to = chatId
      let (message, newState) =
            runState (ServerSate.sendMessage from to date text) state'
      writeTVar state newState
      return (Ok message)
