{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Api.Ping (Ping, ping)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Servant (Application, Capture, Handler, JSON, Post, serve, type (:>))
import Server.Context (Context (Context, state, token))
import Server.Response
  ( Response (Error, Ok, description, parameters),
    ResponseParameters (ResponseParameters, migrateToChatId, retryAfter),
  )
import Server.Token (Token)
import State (State (Dummy))

type Method a = Post '[JSON] (Response a)

type Api =
  Capture "token" Token
    :> ( "ping" :> Method Ping
       )

server :: TVar State -> Token -> Handler (Response Ping)
server state token = return (ping context)
  where
    context = Context {state, token}

app :: IO Application
app = do
  state <- newTVarIO Dummy
  return (serve (Proxy :: Proxy Api) (server state))
