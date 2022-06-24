{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Server (Server, startServer, waitForAction)
import Server.Actions (ActionKind (GetMe))
import qualified Server.Token as Token
import qualified ServerState
import qualified ServerState.BotPermissions as BotPermissions
import ServerState.Id (Id (Id))
import ServerState.InitialBot (InitialBot (InitialBot))
import qualified ServerState.InitialBot as InitialBot
import ServerState.User (User (User))
import qualified ServerState.User as User

botId :: Id
botId = Id 2

runTest :: Server -> IO ()
runTest server = do
  putStrLn "The server is running..."
  waitForAction botId GetMe server
  putStrLn "The bot has called `getMe`. Yay!"
  return ()

main :: IO ()
main = startServer initialState serverSettings runTest
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

    serverSettings = setPort 8081 defaultSettings
