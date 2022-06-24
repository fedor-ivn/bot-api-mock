{-# LANGUAGE OverloadedStrings #-}

module Server (app) where

import Api (Api, api)
import Control.Concurrent (newChan)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.Typeable (Proxy (Proxy))
import GHC.Conc (TVar, newTVarIO)
import Servant (Application, serve)
import qualified Server.Token as Token
import qualified ServerState
import qualified ServerState.BotPermissions as BotPermissions
import ServerState.Id (Id (Id))
import ServerState.InitialBot (InitialBot (InitialBot))
import qualified ServerState.InitialBot as InitialBot
import ServerState.User (User (User))
import qualified ServerState.User as User

app :: IO Application
app = do
  state <- newTVarIO initialState
  serve (Proxy :: Proxy Api) . api state <$> newChan
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
