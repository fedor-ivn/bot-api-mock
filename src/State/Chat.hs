module State.Chat (PrivateChat (PrivateChat)) where

import Data.Map (Map)
import qualified Data.Map as Map
import State.Id
import State.Message (Message)

newtype PrivateChat = PrivateChat [Message]