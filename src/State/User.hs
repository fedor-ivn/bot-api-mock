module State.User (User (..)) where

import Data.Text (Text)
import State.Id (Id)

data User = User
  { id :: Id,
    username :: Maybe Text,
    firstName :: Text,
    lastName :: Maybe Text,
    isBot :: Bool
  }
