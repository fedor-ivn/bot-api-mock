{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (Server, waitForAction, modifyState, startServer) where

import Control.Concurrent (newChan, readChan)
import Control.Monad (unless)
import Control.Monad.State (State, runState)
import Crypto.JOSE (JWK)
import Data.Function ((&))
import Data.Typeable (Proxy(Proxy))
import GHC.Conc (atomically, forkIO, newTVarIO, readTVar, writeTVar)
import Network.Wai.Handler.Warp
    ( Settings
    , runSettings
    , setGracefulShutdownTimeout
    , setInstallShutdownHandler
    )
import Servant (Application, Context((:.), EmptyContext))
import Servant.Auth.Server
    (defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Server (serveWithContext)

import Api (Api, api)
import qualified Api.BotAuth as BotAuth

import Server.Actions (Action(Action), ActionKind)
import Server.Internal (Server(..))
import qualified Server.OneTimeNotifier as OneTimeNotifier

import ServerState (ServerState)
import ServerState.User.Id (UserId)

-- | Wait until the bot performs the expected action.
waitForAction :: UserId -> ActionKind -> Server -> IO ()
waitForAction expectedActor expectedKind Server { actions } = wait
  where
    expectedAction = Action expectedActor expectedKind
    wait = do
        action <- readChan actions
        -- TODO: if we skip some action, it is lost forever. We need to store
        -- skipped actions in some kind of a list
        unless (action == expectedAction) wait

-- | Modify the current server state.
modifyState :: Server -> State ServerState a -> IO a
modifyState Server { stateVar } f = atomically $ do
    state <- readTVar stateVar
    let (returnValue, newState) = runState f state
    writeTVar stateVar newState
    return returnValue

-- | Make an `Application` for the mock Bot API server.
makeApplication :: JWK -> Server -> Application
makeApplication key server@Server { stateVar } = application
  where
    application =
        serveWithContext (Proxy :: Proxy Api) context (api server)
    context =
        defaultCookieSettings
            :. defaultJWTSettings key
            :. BotAuth.Config stateVar
            :. EmptyContext

-- | Start a mock Bot API server with some initial state and settings for the
-- server.
startServer :: ServerState -> Settings -> (Server -> IO ()) -> IO ()
startServer initialState serverSettings runTest = do
    stateVar <- newTVarIO initialState
    actions <- newChan
    let server = Server { stateVar, actions }

    testFinished <- OneTimeNotifier.new
    serverShutDown <- OneTimeNotifier.new
    let handleShutdown closeSocket = do
            _ <- forkIO (OneTimeNotifier.wait testFinished >> closeSocket)
            return ()
    let serverSettings' =
            serverSettings
                & setGracefulShutdownTimeout (Just 1)
                & setInstallShutdownHandler handleShutdown

    key <- generateKey
    _ <- forkIO $ do
        runSettings serverSettings' (makeApplication key server)
        OneTimeNotifier.notify serverShutDown
    runTest server

    OneTimeNotifier.notify testFinished
    OneTimeNotifier.wait serverShutDown
