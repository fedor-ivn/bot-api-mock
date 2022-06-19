{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Api.Ping (Ping, ping)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Servant (Application, Capture, Handler, JSON, Post, serve, type (:>))
import Server.Context (Context (Context, state, token))
import Server.Response (Response)
import Server.Token (Token)
import State (ServerState (ServerState))

type Method a = Post '[JSON] (Response a)

type Api =
  Capture "token" Token
    :> ( "ping" :> Method Ping
       )

server :: TVar ServerState -> Token -> Handler (Response Ping)
server state token = return (ping context)
  where
    context = Context {state, token}

app :: IO Application
app = do
  state <- newTVarIO (ServerState [] Map.empty Map.empty)
  return (serve (Proxy :: Proxy Api) (server state))
