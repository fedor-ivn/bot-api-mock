module State.Message (Message (Message)) where

import Data.Dates
import Data.Text
import State.Id

--import Data.Time.Calendar

data Message = Message
  { id :: Id,
    from :: Id,
    text :: Text,
    date :: DateTime -- need to be confirmed. Is it the right format?
  }