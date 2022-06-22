{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Api.Close (close)
import Api.GetMe (Me, getMe)
import Api.LogOut (logOut)
import Api.Ping (Ping, ping)
import Api.SendMessage (SendMessage, sendMessage)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Servant (Application, Capture, Handler, JSON, Post, ReqBody, Server, serve, type (:<|>) ((:<|>)), type (:>))
import Server.Context (Context (..))
import Server.Response (Response)
import Server.Token (Token)
import qualified Server.Token as Token
import ServerState (ServerState)
import qualified ServerState
import qualified ServerState.BotPermissions as BotPermissions
import ServerState.Id (Id (Id))
import ServerState.InitialBot (InitialBot (InitialBot))
import qualified ServerState.InitialBot as InitialBot
import ServerState.Message (Message (..))
import ServerState.User (User (User))
import qualified ServerState.User as User

type Method a = Post '[JSON] (Response a)

type Api =
  Capture "token" Token
    :> ( "ping" :> Method Ping
           :<|> "getMe" :> Method Me
           :<|> "logOut" :> Method Bool
           :<|> "close" :> Method Bool
           :<|> "sendMessage" :> ReqBody '[JSON] SendMessage :> Method Message
       )

server :: TVar ServerState -> Server Api
server state token =
  return (ping context)
    :<|> getMe context
    :<|> logOut
    :<|> close
    :<|> sendMessage context
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
        { -- SAFETY: `Token.parse` is called on a literal, so we can guarantee
          -- that it returns `Just`. Calling `fromJust` on a `Just` is safe.
          InitialBot.token = fromJust (Token.parse "2:random-characters"),
          InitialBot.name = "Bot",
          InitialBot.username = "a_bot",
          InitialBot.permissions = BotPermissions.defaultPermissions
        }
