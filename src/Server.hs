{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (Server, startServer) where

import Api (Api, api)
import Control.Concurrent (newChan)
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Servant (Application, serve)
import Server.Actions (Actions)
import ServerState (ServerState)

-- | A mock Bot API server.
data Server = Server
  { stateVar :: TVar ServerState,
    actions :: Actions
  }

-- | Make an `Application` for the mock Bot API server.
makeApplication :: Server -> Application
makeApplication Server {stateVar, actions} =
  serve (Proxy :: Proxy Api) (api stateVar actions)

-- | Start a mock Bot API server with some initial state and settings for the
-- server.
startServer :: ServerState -> Settings -> IO ()
startServer initialState serverSettings = do
  stateVar <- newTVarIO initialState
  actions <- newChan
  let server = Server {stateVar, actions}
  let application = makeApplication server
  runSettings serverSettings application
