{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState.CompleteMessage (CompleteMessage(..)) where

import Data.Aeson ((.=), ToJSON(toJSON), object)

import ServerState.Chat (Chat)
import ServerState.Message (Message(Message))
import qualified ServerState.Message as Message
import ServerState.User (User)

-- | A `Message` which can be serialized.
--
-- The `Message` datatype contains only the very necessary information about a
-- message. Before returning a message to the bot, it must be enriched with
-- additional information, which, however, would be duplicated if stored as-is
-- in `ServerState`.

data CompleteMessage = CompleteMessage
    { message :: Message
    , chat :: Chat
    , from :: User
    }

instance ToJSON CompleteMessage where
    toJSON CompleteMessage { message, chat, from } = object
        [ "message_id" .= messageId
        , "from" .= from
        , "date" .= date
        , "chat" .= chat
        , "text" .= text
        ]
      where
        Message { Message.messageId, Message.date, Message.text } = message
