{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.SendMessage (SendMessage, sendMessage) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, runState)
import Data.Aeson
    ( FromJSON(parseJSON)
    , Options(fieldLabelModifier, omitNothingFields)
    , camelTo2
    , defaultOptions
    , genericParseJSON
    )
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import GHC.Conc (atomically, readTVar, writeTVar)
import GHC.Generics (Generic)
import Servant (Handler)

import Server.Actions (writeAction)
import qualified Server.Actions as Actions
import Server.Context (Context(..))
import Server.Response (Response(Ok))

import qualified ServerState
import ServerState (ServerState)
import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Id (Id)
import ServerState.Time (Time(Time))
import ServerState.User (User(User))
import qualified ServerState.User as User

data SendMessage = SendMessage
    { chatId :: Id
    , text :: Text
    }
    deriving Generic

instance FromJSON SendMessage where
    parseJSON = genericParseJSON options
      where
        options = defaultOptions
            { fieldLabelModifier = camelTo2 '_'
            , omitNothingFields = True
            }

sendMessage'
    :: SendMessage
    -> User
    -> Time
    -> State ServerState (Response CompleteMessage)
sendMessage' parameters botUser currentTime =
    Ok <$> ServerState.sendMessage from to currentTime text
  where
    SendMessage { chatId = to, text } = parameters
    User { User.userId = from } = botUser

sendMessage :: Context -> SendMessage -> Handler (Response CompleteMessage)
sendMessage Context { state, botUser, actions } parameters = do
    date <- Time <$> liftIO getCurrentTime
    writeAction (User.userId botUser) actions Actions.SendMessage
    liftIO $ atomically $ do
        state' <- readTVar state
        let (response, updatedState) =
                runState (sendMessage' parameters botUser date) state'
        writeTVar state updatedState
        return response
