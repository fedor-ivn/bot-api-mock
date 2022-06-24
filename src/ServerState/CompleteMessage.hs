{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState.CompleteMessage (CompleteMessage (..)) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import ServerState.Chat (Chat)
import ServerState.Message (Message (Message))
import qualified ServerState.Message as Message
import ServerState.User (User)

data CompleteMessage = CompleteMessage
  { message :: Message,
    chat :: Chat,
    from :: User
  }

instance ToJSON CompleteMessage where
  toJSON
    CompleteMessage
      { message = Message {Message.id, Message.date, Message.text},
        chat,
        from
      } =
      object
        [ "message_id" .= id,
          "from" .= from,
          "date" .= date,
          "chat" .= chat,
          "text" .= text
        ]
