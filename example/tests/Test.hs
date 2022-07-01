{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import System.Process (CreateProcess(env), createProcess, proc)

import Server (startServer, waitForAction)
import Server.Actions (ActionKind(SendMessage))
import qualified Server.Token as Token

import ServerState (ServerState)
import qualified ServerState
import qualified ServerState.BotPermissions as BotPermissions
import ServerState.Id (Id(Id))
import ServerState.InitialBot (InitialBot(InitialBot))
import qualified ServerState.InitialBot as InitialBot
import ServerState.User (User(User))
import qualified ServerState.User as User

botToken :: Text
botToken = "2:random-characters"

botId :: Id
botId = Id 2

initialState :: ServerState
initialState = ServerState.initialize (user :| []) (bot :| [])
  where
    user = User
        { User.userId = Id 1
        , User.firstName = "User"
        , User.lastName = Nothing
        , User.username = Nothing
        , User.isBot = False
        }
    bot = InitialBot
        { -- SAFETY: `Token.parse` is called on a constant, so we can guarantee
          -- that it returns `Just`. Calling `fromJust` on a `Just` is safe.
          InitialBot.token = fromJust (Token.parse botToken)
        , InitialBot.name = "Bot"
        , InitialBot.username = "a_bot"
        , InitialBot.permissions = BotPermissions.defaultPermissions
        }

serverSettings :: Settings
serverSettings = setPort 8081 defaultSettings

botPath :: FilePath
botPath = "./bot/target/release/bot"

botEnv :: [(String, String)]
botEnv =
    [ ("BOT_TOKEN", Text.unpack botToken)
    , ("BOT_API_URI", "http://localhost:8081/")
    ]

startBot :: IO ()
startBot = do
    _ <- createProcess ((proc botPath []) { env = Just botEnv })
    return ()

main :: IO ()
main = startServer initialState serverSettings $ \server -> do
    startBot
    waitForAction botId SendMessage server
    putStrLn "The bot has called `getMe`. Yay!"
    return ()
