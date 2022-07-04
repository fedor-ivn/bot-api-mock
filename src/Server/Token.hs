{-# LANGUAGE OverloadedStrings #-}

module Server.Token (Token, parse, getId) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Servant (FromHttpApiData(parseUrlPiece))
import ServerState.User.Id (UserId(UserId))

-- | A bot's token.
data Token = Token UserId Text
    deriving Eq

instance FromHttpApiData Token where
    parseUrlPiece piece = case parsedToken of
        Nothing -> Left "Failed to parse bot token"
        Just token -> Right token
        where parsedToken = Text.stripPrefix "bot" piece >>= parse

-- | Parses a token.
--
-- >>> import Data.Maybe
-- >>> isJust (parseToken "123456:9ff811aad7a5532679bc908fc1429f3d360")
-- True
-- >>> isJust (parseToken "blahblah")
-- False
parse :: Text -> Maybe Token
parse unparsedToken = do
    [unparsedId, hash] <- return (Text.split (== ':') unparsedToken)
    botId <- readMaybe (Text.unpack unparsedId)
    return (Token (UserId botId) hash)

-- | Extract the bot's ID from its token.
--
-- >>> fmap getId (parseToken "123456:9ff811aad7a5532679bc908fc1429f3d360")
-- Just (UserId 123456)
getId :: Token -> UserId
getId (Token botId _) = botId
