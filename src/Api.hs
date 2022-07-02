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
import Servant.Auth.Server (Auth, AuthResult(Authenticated), ThrowAll(throwAll))

import Api.BotAuth (BotAuth, BotInfo(..))
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

import Api.DeleteWebhook (deleteWebhook)
import ServerState (ServerState)
import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Update (Update)

-- | An alias for method return value.
type Method a = Post '[JSON] (Response a)

-- | A scheme of Bot API.
type Api =
    Auth '[BotAuth] BotInfo
        :> Capture "token" Token
        :> (
            "ping" :> Method Ping
            :<|> "getMe" :> Method Me
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

-- | Make a mock Bot API server.
api :: TVar ServerState -> Actions -> Server Api
api state actions (Authenticated (BotInfo bot botUser)) _ =
    return (ping context)
        :<|> getMe context
        :<|> logOut context
        :<|> close context
        :<|> deleteWebhook context
        :<|> sendMessage context
        :<|> getUpdates context
  where
    context = Context
        { Context.state = state
        , Context.bot = bot
        , Context.botUser = botUser
        , Context.actions = actions
        }
api _ _ _ _ = throwAll err401
