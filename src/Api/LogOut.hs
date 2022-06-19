module Api.LogOut where

import Server.Response (Response (Ok))
import Servant (Handler)

logOut :: Handler (Response Bool)
logOut = return (Ok True)