{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Api.Close (close)
import Api.GetMe (Me, getMe)
import Api.LogOut (logOut)
import Api.Ping (Ping, ping)
import Api.SendMessage (SendMessage, sendMessage)
import Control.Monad.IO.Class (liftIO)
import Data.Dates (getCurrentDateTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Servant (Application, Capture, Handler, JSON, Post, ReqBody, Server, serve, type (:<|>) ((:<|>)), type (:>))
import Server.Context (Context (..))
import Server.Response (Response)
import Server.Token (Token)
import ServerState (ServerState (ServerState))
import ServerState.Id (Id (Id))
import ServerState.Message (Message)
import ServerState.User (User (..))
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
  state <- newTVarIO (ServerState [] Map.empty Map.empty)
  return (serve (Proxy :: Proxy Api) (server state))
