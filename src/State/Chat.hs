module State.Chat(PrivateChat (PrivateChat)) where
import Data.Map (Map)
import qualified Data.Map as Map
import State.Message(Message)
import State.Id

newtype PrivateChat = PrivateChat [Message]