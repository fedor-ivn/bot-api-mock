{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState.Chat (Chat (..)) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Text (Text)
import ServerState.User (User (User))
import qualified ServerState.User as User

-- | Any chat (so far only a private chat).
newtype Chat = PrivateChat User

instance ToJSON Chat where
  toJSON
    ( PrivateChat
        User
          { User.userId,
            User.firstName,
            User.lastName,
            User.username
          }
      ) =
      object
        ( [ "type" .= ("private" :: Text),
            "id" .= userId,
            "first_name" .= firstName
          ]
            ++ optional "last_name" lastName
            ++ optional "username" username
        )
      where
        optional name (Just x) = [name .= x]
        optional _ Nothing = []
