{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.SendMessage (SendMessage, sendMessage) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (runState)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
    genericParseJSON,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import GHC.Conc (atomically, readTVar, writeTVar)
import GHC.Generics (Generic)
import Servant (Handler)
import qualified Server.Actions as Actions
import Server.Actions (writeAction)
import Server.Context (Context (..))
import Server.Response (Response (Ok))
import Server.Token (Token)
import qualified Server.Token as Token
import ServerState (ServerState)
import qualified ServerState
import ServerState.Id (Id (Id))
import ServerState.Message (Message)
import ServerState.Time (Time (Time))

data SendMessage = SendMessage
  { chatId :: Id,
    text :: Text
  }
  deriving (Generic)

instance FromJSON SendMessage where
  parseJSON = genericParseJSON options
    where
      options =
        defaultOptions
          { fieldLabelModifier = camelTo2 '_',
            omitNothingFields = True
          }

sendMessage :: Context -> SendMessage -> Handler (Response Message)
sendMessage Context {state, token, actions} SendMessage {chatId, text} = do
  date <- Time <$> liftIO getCurrentTime
  writeAction token actions Actions.SendMessage
  liftIO $
    atomically $ do
      state' <- readTVar state
      let from = Token.getId token
      let to = chatId
      let (message, newState) =
            runState (ServerState.sendMessage from to date text) state'
      writeTVar state newState
      return (Ok message)
