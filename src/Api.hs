{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Api (Api, api) where

import GHC.Conc (TVar)
import Servant
    ( type (:<|>)((:<|>))
    , type (:>)
    , Capture
    , JSON
    , Post
    , ReqBody
    , Server
    , err401
    )
import Servant.Auth.Server
    ( Auth
    , AuthResult(Authenticated)
    , ThrowAll(throwAll)
    )

import Api.Close (close)
import Api.GetMe (Me, getMe)
import Api.GetUpdates (GetUpdates, getUpdates)
import Api.LogOut (logOut)
import Api.Ping (Ping, ping)
import Api.SendMessage (SendMessage, sendMessage)

import Server.Actions (Actions)
import Server.Context (Context(Context))
import qualified Server.Context as Context
import Server.Response (Response)
import Server.Token (Token)
import ServerState (ServerState)
import ServerState.Bot (Bot)
import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Update (Update)
import Api.BotAuth (BotAuth)

type Method a = Post '[JSON] (Response a)

type Api =
    Auth '[BotAuth] Bot
        :> Capture "token" Token
        :> (
            "ping" :> Method Ping
            :<|> "getMe" :> Method Me
            :<|> "logOut" :> Method Bool
            :<|> "close" :> Method Bool
            :<|> (
                "sendMessage"
                    :> ReqBody '[JSON] SendMessage
                    :> Method CompleteMessage
            )
            :<|> (
                "getUpdates"
                    :> ReqBody '[JSON] GetUpdates
                    :> Method [Update]
            )
        )

api :: TVar ServerState -> Actions -> Server Api
api state actions (Authenticated _bot) token =
    return (ping context)
        :<|> getMe context
        :<|> logOut context
        :<|> close context
        :<|> sendMessage context
        :<|> getUpdates context
  where
    context = Context
        { Context.state = state
        , Context.token = token
        , Context.actions = actions
        }
api _ _ _ _ = throwAll err401
