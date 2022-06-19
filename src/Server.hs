{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Api.Ping (Ping, ping)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Servant (Application, Capture, Handler, JSON, Post, serve, type (:>), type (:<|>) ((:<|>)), Server)
import Server.Context (Context (Context, state, token))
import Server.Response (Response)
import Server.Token (Token)
import ServerState (ServerState (ServerState))
import ServerState.Id (Id (Id))
import ServerState.User (User (User))
import qualified ServerState.User as User
import Api.GetMe (getMe)

type Method a = Post '[JSON] (Response a)

type Api =
  Capture "token" Token
    :> ( "ping" :> Method Ping
    :<|> "getMe":> Method User
       )

server :: TVar ServerState -> Server Api
server state token = return (ping context) :<|> getMe context
  where
    context = Context {state, token}

app :: IO Application
app = do
  state <- newTVarIO (ServerState [] Map.empty Map.empty)
  return (serve (Proxy :: Proxy Api) (server state))
