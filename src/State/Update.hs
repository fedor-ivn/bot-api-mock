module State.Update (Update (..)) where

import State.Id (Id)
import State.Message (Message)

data Update = Update
  { id :: Id,
    message :: Message
  }
