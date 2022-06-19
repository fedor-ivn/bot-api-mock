module ServerState.InitialBot (InitialBot (..)) where

import Data.Text (Text)
import Server.Token (Token)
import ServerState.BotPermissions (BotPermissions)
import ServerState.Id (Id)

-- | Information about a bot. This datatype is used to initialize `ServerState`.
data InitialBot = InitialBot
  { token :: Token,
    name :: Text,
    username :: Text,
    permissions :: BotPermissions
  }
  deriving (Eq)
