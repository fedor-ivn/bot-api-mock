module ServerState.Update (Update (..)) where

import ServerState.Id (Id)
import ServerState.Message (Message)

data Update = Update
  { id :: Id,
    message :: Message
  }
