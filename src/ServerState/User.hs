module ServerState.User (User (..)) where

import Data.Text (Text)
import ServerState.Id (Id)

data User = User
  { id :: Id,
    username :: Maybe Text,
    firstName :: Text,
    lastName :: Maybe Text,
    isBot :: Bool
  }
