{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Api.Ping (Ping, ping)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Proxy (Proxy))
import Servant (Application, Capture, Handler, JSON, Post, serve, type (:>))
import Server.Context (Context (Context, token))
import Server.Response
  ( Response (Error, Ok, description, parameters),
    ResponseParameters (ResponseParameters, migrateToChatId, retryAfter),
  )
import Server.Token (Token)

type Method a = Post '[JSON] (Response a)

type Api =
  Capture "token" Token
    :> ( "ping" :> Method Ping
       )

server :: Token -> Handler (Response Ping)
server token = return (ping context)
  where
    context = Context {token}

app :: Application
app = serve (Proxy :: Proxy Api) server
