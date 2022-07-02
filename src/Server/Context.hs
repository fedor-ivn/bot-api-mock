module Server.Context (Context(..)) where

import Server.Internal (Server)

import ServerState.Bot (Bot)
import ServerState.User (User)

data Context = Context
    { server :: Server
    , bot :: Bot
    , botUser :: User
    }
