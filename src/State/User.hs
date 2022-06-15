module State.User (User (User)) where
import Server.Token (Token)
import State.Id
import Data.String (String)

data User = User
    {
        id :: Id,
        username :: String,
        firstName :: String,
        surname :: String
    }

