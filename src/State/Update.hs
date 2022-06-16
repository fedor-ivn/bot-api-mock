module State.Update (Update (Update)) where

import State.Id
import State.Message

data Update = Update
  { id :: Id,
    message :: Message
  }