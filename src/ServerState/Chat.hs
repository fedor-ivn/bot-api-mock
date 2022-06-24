{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState.Chat (Chat (..)) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import ServerState.User (User (User))
import qualified ServerState.User as User

-- | Any chat (so far only a private chat).
newtype Chat = PrivateChat User

instance ToJSON Chat where
  toJSON
    ( PrivateChat
        User
          { User.id,
            User.firstName,
            User.lastName,
            User.username
          }
      ) =
      object
        ( [ "type" .= ("private" :: Text),
            "id" .= id,
            "first_name" .= firstName
          ]
            ++ optional "last_name" lastName
            ++ optional "username" username
        )
      where
        optional name (Just x) = [name .= x]
        optional _ Nothing = []
