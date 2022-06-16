module State.User (User (User)) where

import Data.String (String)
import Data.Text (Text)
import Server.Token (Token)
import State.Id

data User = User
  { id :: Id,
    username :: Maybe Text,
    firstName :: Text,
    lastName :: Maybe Text,
    isBot :: Bool
  }
