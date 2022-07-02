{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState.CompleteMessage (CompleteMessage(..)) where

import Data.Aeson ((.=), ToJSON(toJSON), object)

import ServerState.Chat (Chat)
import ServerState.Message (Message(Message))
import qualified ServerState.Message as Message
import ServerState.User (User)

-- | Information about the message that is to be returned by `getUpdates`.
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
