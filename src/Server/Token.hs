{-# LANGUAGE OverloadedStrings #-}

module Server.Token (Token (Token), getId) where

import Data.Text (Text)
import qualified Data.Text as Text
import Servant (FromHttpApiData (parseQueryParam, parseUrlPiece))
import State.Id (Id(..))
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

-- | A bot's token.
newtype Token = Token Text deriving (Eq)

instance FromHttpApiData Token where
  parseUrlPiece piece = case Text.stripPrefix "bot" piece of
    Nothing -> Left "Failed to parse bot token"
    Just token -> Right (Token token)

-- | Extract the bot's ID from its token.
--
-- >>> getId (Token "123456:gfdhtggdgfdghfd")
-- Just (Id 123456)
-- >>> getId (Token ("foooo"))
-- Nothing
getId :: Token -> Maybe Id
getId (Token token) = do
  rawId <- listToMaybe (Text.split (==':') token)
  id <- readMaybe (Text.unpack rawId)
  return (Id id)
