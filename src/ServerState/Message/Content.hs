{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module ServerState.Message.Content
    ( Entity(..)
    , EntityKind(..)
    , Content(..)
    , makeMessageContent
    ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Prelude hiding (length)
import Text.RE.Replace (Capture(..), matchCapture)
import Text.RE.TDFA.Text ((*=~), Matches(allMatches), RE, re)

-- An entity inside a message.
data Entity = Entity
    { kind :: EntityKind
    , offset :: Int
    , length :: Int
    }
    deriving Show

-- The kind of an entity.
data EntityKind = BotCommand
    deriving (Eq, Show)

-- The text of a message with its entities.
data Content = Content
    { text :: Text
    , entities :: [Entity]
    }

commandRegex :: RE
commandRegex = [re|/[a-z][a-z_]{0,31}|]

-- | Extract commands inside a message.
--
-- # Examples
--
-- >>> parseCommands "i say /hello"
-- [Entity {kind = BotCommand, offset = 6, length = 6}]
parseCommands :: Text -> [Entity]
parseCommands message = enitities
  where
    matches = allMatches (message *=~ commandRegex)
    enitities = mapMaybe matchToEntity matches
    matchToEntity match = case matchCapture match of
        Nothing -> Nothing
        Just Capture { captureOffset, captureLength } -> Just $ Entity
            { kind = BotCommand
            , offset = captureOffset
            , length = captureLength
            }

-- | Extract all entities inside a message.
parseEntities :: Text -> [Entity]
parseEntities message = entities where entities = parseCommands message

-- | Parse entities inside the message and return `Message.Content`.
makeMessageContent :: Text -> Content
makeMessageContent text = Content { text, entities }
    where entities = parseEntities text
