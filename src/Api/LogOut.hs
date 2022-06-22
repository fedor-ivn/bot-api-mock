module Api.LogOut (logOut) where

import Servant (Handler)
import Server.Response (Response (Ok))

logOut :: Handler (Response Bool)
logOut = return (Ok True)
