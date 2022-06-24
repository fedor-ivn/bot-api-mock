{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (Api, api) where

import Api.Close (close)
import Api.GetMe (Me, getMe)
import Api.LogOut (logOut)
import Api.Ping (Ping, ping)
import Api.SendMessage (SendMessage, sendMessage)
import GHC.Conc (TVar)
import Servant
  ( Capture,
    JSON,
    Post,
    ReqBody,
    Server,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Server.Actions (Actions)
import Server.Context (Context (Context))
import qualified Server.Context as Context
import Server.Response (Response)
import Server.Token (Token)
import ServerState (ServerState)
import ServerState.Message (Message)

type Method a = Post '[JSON] (Response a)

type Api =
  Capture "token" Token
    :> ( "ping" :> Method Ping
           :<|> "getMe" :> Method Me
           :<|> "logOut" :> Method Bool
           :<|> "close" :> Method Bool
           :<|> "sendMessage" :> ReqBody '[JSON] SendMessage :> Method Message
       )

api :: TVar ServerState -> Actions -> Server Api
api state actions token =
  return (ping context)
    :<|> getMe context
    :<|> logOut context
    :<|> close context
    :<|> sendMessage context
  where
    context =
      Context
        { Context.state = state,
          Context.token = token,
          Context.actions = actions
        }