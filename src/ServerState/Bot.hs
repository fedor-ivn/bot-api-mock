module ServerState.Bot (Bot (..)) where

import Data.Sequence (Seq)
import Server.Token (Token)
import ServerState.BotPermissions (BotPermissions)
import ServerState.Update (Update)

-- | Additional information about a bot.
data Bot = Bot
  { token :: Token,
    permissions :: BotPermissions,
    updates :: Seq Update
  }
