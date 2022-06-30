{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.BotAuth (BotAuth, Config(..)) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Conc (TVar, atomically, readTVar)
import Network.Wai (Request(rawPathInfo))
import Servant (FromHttpApiData(parseUrlPiece))
import Servant.Auth.Server (AuthCheck(AuthCheck), AuthResult(..))
import Servant.Auth.Server.Internal.Class (IsAuth(AuthArgs, runAuth))

import qualified Server.Token as Token

import Control.Monad.State (evalState)
import ServerState (ServerState)
import qualified ServerState
import ServerState.Bot (Bot)
import qualified ServerState.Bot as Bot

data BotAuth

newtype Config = Config (TVar ServerState)

botAuthCheck :: Config -> AuthCheck Bot
botAuthCheck (Config stateVar) = AuthCheck $ \request -> do
    let path = decodeUtf8 (rawPathInfo request)
        tokenParseResult = do
            rawToken <- case Text.split (== '/') path of
                ("" : token : _) -> Right token
                _ -> Left "Failed to extract the token"
            parseUrlPiece rawToken

    case tokenParseResult of
        Left _ -> return Indefinite
        Right token -> atomically $ do
            state <- readTVar stateVar
            let
                getBot = do
                    bots <- ServerState.getBots
                    let bot = Map.lookup (Token.getId token) bots
                    return bot

            case evalState getBot state of
                Nothing -> return NoSuchUser
                Just bot' -> if Bot.token bot' == token
                    then return (Authenticated bot')
                    else return BadPassword

instance IsAuth BotAuth Bot where
    type AuthArgs BotAuth = '[Config]
    runAuth _ _ = botAuthCheck
