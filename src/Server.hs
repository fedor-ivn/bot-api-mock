{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Api.Ping (Ping, ping)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Servant (Application, Capture, Handler, JSON, Post, serve, type (:>))
import Server.Context (Context (Context, state, token))
import Server.Response (Response)
import Server.Token (Token (Token))
import ServerState (ServerState)
import qualified ServerState
import qualified ServerState.BotPermissions as BotPermissions
import ServerState.Id (Id (Id))
import ServerState.InitialBot (InitialBot (InitialBot))
import qualified ServerState.InitialBot as InitialBot
import ServerState.User (User (User))
import qualified ServerState.User as User

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
  state <- newTVarIO initialState
  return (serve (Proxy :: Proxy Api) (server state))
  where
    initialState = ServerState.initialize (user :| []) (bot :| [])
    user =
      User
        { User.id = Id 1,
          User.firstName = "User",
          User.lastName = Nothing,
          User.username = Nothing,
          User.isBot = False
        }
    bot =
      InitialBot
        { InitialBot.token = Token "2:random-characters",
          InitialBot.name = "Bot",
          InitialBot.username = "a_bot",
          InitialBot.permissions = BotPermissions.defaultPermissions
        }
