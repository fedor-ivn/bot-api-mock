{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (Server, waitForAction, startServer) where

import Api (Api, api)
import Control.Concurrent (newChan, readChan)
import Control.Monad (unless)
import Data.Function ((&))
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, forkIO, newTVarIO)
import Network.Wai.Handler.Warp
  ( Settings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
  )
import Servant (Application, serve)
import Server.Actions (Action (Action), ActionKind, Actions)
import qualified Server.OneTimeNotifier as OneTimeNotifier
import ServerState (ServerState)
import ServerState.Id (Id)

-- | A mock Bot API server.
data Server = Server
  { stateVar :: TVar ServerState,
    actions :: Actions
  }

-- | Wait until the bot performs the expected action.
waitForAction :: Id -> ActionKind -> Server -> IO ()
waitForAction expectedActor expectedKind Server {actions} = wait
  where
    expectedAction = Action expectedActor expectedKind
    wait = do
      action <- readChan actions
      -- TODO: if we skip some action, it is lost forever. We need to store
      -- skipped actions in some kind of a list
      unless (action == expectedAction) wait

-- | Make an `Application` for the mock Bot API server.
makeApplication :: Server -> Application
makeApplication Server {stateVar, actions} =
  serve (Proxy :: Proxy Api) (api stateVar actions)

-- | Start a mock Bot API server with some initial state and settings for the
-- server.
startServer :: ServerState -> Settings -> (Server -> IO ()) -> IO ()
startServer initialState serverSettings runTest = do
  stateVar <- newTVarIO initialState
  actions <- newChan
  let server = Server {stateVar, actions}

  testFinished <- OneTimeNotifier.new
  serverShutDown <- OneTimeNotifier.new
  let handleShutdown closeSocket = do
        forkIO (OneTimeNotifier.wait testFinished >> closeSocket)
        return ()
  let serverSettings' =
        serverSettings
          & setGracefulShutdownTimeout (Just 1)
          & setInstallShutdownHandler handleShutdown

  forkIO $ do
    runSettings serverSettings' (makeApplication server)
    OneTimeNotifier.notify serverShutDown
  runTest server

  OneTimeNotifier.notify testFinished
  OneTimeNotifier.wait serverShutDown
