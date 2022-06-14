module State.Message(Message (Message)) where
import Data.Text
import State.Id

data Message = Message
    {
        id :: Id,
        text :: Text
    }