module Api.Close (close) where

import Server.Response (Response (Ok))
import Servant (Handler)

close :: Handler (Response Bool)
close = return (Ok True)
