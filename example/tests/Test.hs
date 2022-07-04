{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import System.Process
    (CreateProcess(env), ProcessHandle, createProcess, proc, terminateProcess)

import Server (Server, modifyState, startServer, waitForAction)
import Server.Actions (ActionKind(SendMessage))
import qualified Server.Token as Token

import Data.Time (getCurrentTime)
import ServerState (ServerState)
import qualified ServerState
import qualified ServerState.BotPermissions as BotPermissions
import ServerState.InitialBot (InitialBot(InitialBot))
import qualified ServerState.InitialBot as InitialBot
import ServerState.Time (Time(Time))
import ServerState.User (User(User))
import qualified ServerState.User as User
import ServerState.User.Id (UserId(UserId))

userId :: UserId
userId = UserId 1

botToken :: Text
botToken = "2:random-characters"

botId :: UserId
botId = UserId 2

initialState :: ServerState
initialState = ServerState.initialize (user :| []) (bot :| [])
  where
    user = User
        { User.userId = userId
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

startBot :: IO ProcessHandle
startBot = do
    (_, _, _, process) <- createProcess
        ((proc botPath []) { env = Just botEnv })
    return process

killBot :: ProcessHandle -> IO ()
killBot = terminateProcess

sendMessage :: Text -> Server -> IO ()
sendMessage text server = do
    currentTime <- Time <$> getCurrentTime
    modifyState server $ do
        _ <- ServerState.sendMessage userId botId currentTime text
        return ()

main :: IO ()
main = startServer initialState serverSettings $ \server -> do
    bot <- startBot

    sendMessage "/start" server
    waitForAction botId SendMessage server
    putStrLn "The bot has replied to /start. Good!"

    sendMessage "Hello!" server
    waitForAction botId SendMessage server
    putStrLn "The bot has replied to a text message. Great!"

    killBot bot
    return ()
