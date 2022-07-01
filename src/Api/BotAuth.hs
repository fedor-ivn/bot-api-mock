{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.BotAuth (BotAuth, BotInfo(..), Config(..)) where

import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Conc (TVar, atomically, readTVar)
import Network.Wai (Request(rawPathInfo))
import Servant (FromHttpApiData(parseUrlPiece))
import Servant.Auth.Server
    (AuthCheck(AuthCheck), AuthResult(..), ToJWT(encodeJWT))
import Servant.Auth.Server.Internal.Class (IsAuth(AuthArgs, runAuth))

import qualified Server.Token as Token

import Control.Applicative (Applicative(liftA2))
import Control.Monad.State (evalState)
import Crypto.JWT (emptyClaimsSet)
import ServerState (ServerState)
import qualified ServerState
import ServerState.Bot (Bot)
import qualified ServerState.Bot as Bot
import ServerState.User (User)

data BotAuth

data BotInfo = BotInfo Bot User

newtype Config = Config (TVar ServerState)

botAuthCheck :: Config -> AuthCheck BotInfo
botAuthCheck (Config stateVar) = AuthCheck $ \request -> do
    let path = decodeUtf8 (rawPathInfo request)
    let tokenParseResult = do
            rawToken <- case Text.split (== '/') path of
                ("" : token : _) -> Right token
                _ -> Left "Failed to extract the token"
            parseUrlPiece rawToken

    case tokenParseResult of
        Left _ -> return Indefinite
        Right token -> atomically $ do
            state <- readTVar stateVar
            let botId = Token.getId token
                getBot = do
                    user <- ServerState.getUser botId
                    bot <- ServerState.getBot botId
                    return (liftA2 (,) bot user)

            case evalState getBot state of
                Nothing -> return NoSuchUser
                Just (bot, user) -> if Bot.token bot == token
                    then return (Authenticated (BotInfo bot user))
                    else return BadPassword

instance IsAuth BotAuth BotInfo where
    type AuthArgs BotAuth = '[Config]
    runAuth _ _ = botAuthCheck

instance ToJWT BotInfo where
    encodeJWT _ = emptyClaimsSet
