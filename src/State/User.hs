module State.User (User (User)) where
import Server.Token (Token)
import State.Id

data User = User Id (Maybe Token)
