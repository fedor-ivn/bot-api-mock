module State (State (State)) where
import State.User
import State.Chat
import Data.Sequence
import Data.Sequence.Internal.Sorting (Queue)
import State.Update (Update)
import State.Id
import Data.Map (Map)
import qualified Data.Map as Map
import State.Message(Message)
import Server.Token (Token)

-- TODO: Implement real state
data State = State 
    {
        users :: [User],
        chats :: [Chat],
        forBots :: Map Int BotHandler
    }

data BotHandler = BotHandler
    {
        token :: Token,
        updatesTohadle :: Queue Update
    }

-- User
-- [Chat] -> Chat Map<(user1Id,user2Id) : [Message]> 
-- Map <botid : {token::Token, updatesTohadle :: Queue Update}>  


-- data Update = { id :: integer, message :: Message (to be cont.)}

-- getUser :: State -> Id -> Maybe User
-- sendMessage :: State -> Id -> Id -> Text -> State