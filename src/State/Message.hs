module State.Message (Message (..)) where

import Data.Dates ( DateTime )
import Data.Text ( Text )
import State.Id ( Id )

data Message = Message
  { id :: Id,
    from :: Id,
    text :: Text,
    date :: DateTime
  }