module State.Chat where
import Data.Map (Map)
import qualified Data.Map as Map
import State.Message(Message)
import State.Id

data Chat = Chat (Map (Id,Id) [Message])