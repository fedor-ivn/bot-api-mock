{-# LANGUAGE OverloadedStrings #-}

module Server (startServer) where

import Api (Api, api)
import Control.Concurrent (newChan)
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Servant (Application, serve)
import Server.Actions (Actions)
import ServerState (ServerState)

-- | Make an `Application` for the mock Bot API server.
makeApplication :: TVar ServerState -> Actions -> Application
makeApplication state actions = serve (Proxy :: Proxy Api) (api state actions)

-- | Start a mock Bot API server with some initial state and settings for the
-- server.
startServer :: ServerState -> Settings -> IO ()
startServer initialState serverSettings = do
  state <- newTVarIO initialState
  actions <- newChan
  let application = makeApplication state actions
  runSettings serverSettings application
