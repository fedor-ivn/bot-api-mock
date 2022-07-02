{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Api (Api, api) where

import qualified Servant
import Servant
    (type (:<|>)((:<|>)), type (:>), Capture, JSON, Post, ReqBody, err401)
import Servant.Auth.Server (Auth, AuthResult(Authenticated), ThrowAll(throwAll))

import Api.BotAuth (BotAuth, BotInfo(..))
import Api.Close (close)
import Api.DeleteWebhook (deleteWebhook)
import Api.GetMe (Me, getMe)
import Api.GetUpdates (GetUpdates, getUpdates)
import Api.LogOut (logOut)
import Api.SendMessage (SendMessage, sendMessage)

import Server.Context (Context(Context))
import qualified Server.Context as Context
import Server.Internal (Server)
import Server.Response (Response)
import Server.Token (Token)

import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Update (Update)

type Method a = Post '[JSON] (Response a)

type Api =
    Auth '[BotAuth] BotInfo
        :> Capture "token" Token
        :> (
            "getMe" :> Method Me
            :<|> "logOut" :> Method Bool
            :<|> "close" :> Method Bool
            :<|> "deleteWebhook" :> Method Bool
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

api :: Server -> Servant.Server Api
api server (Authenticated (BotInfo bot botUser)) _ =
    getMe context
    :<|> logOut context
    :<|> close context
    :<|> deleteWebhook context
    :<|> sendMessage context
    :<|> getUpdates context
  where
    context = Context
        { Context.server = server
        , Context.bot = bot
        , Context.botUser = botUser
        }
api _ _ _ = throwAll err401
