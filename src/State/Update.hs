module State.Update(Update(Update)) where 
import State.Message
import State.Id

data Update = Update
    {
        id :: Id,
        message :: Message
    }