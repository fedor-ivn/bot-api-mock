{-# LANGUAGE OverloadedStrings #-}

module Server.Token (Token, parse, getId) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Servant (FromHttpApiData (parseQueryParam, parseUrlPiece))
import ServerState.Id (Id (Id))
import Text.Read (readMaybe)

-- | A bot's token.
data Token = Token Id Text deriving (Eq)

instance FromHttpApiData Token where
  parseUrlPiece piece = case parsedToken of
    Nothing -> Left "Failed to parse bot token"
    Just token -> Right token
    where
      parsedToken = Text.stripPrefix "bot" piece >>= parse

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
  id <- readMaybe (Text.unpack unparsedId)
  return (Token (Id id) hash)

-- | Extract the bot's ID from its token.
--
-- >>> fmap getId (parseToken "123456:9ff811aad7a5532679bc908fc1429f3d360")
-- Just (Id 123456)
getId :: Token -> Id
getId (Token id _) = id
