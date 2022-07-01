module Server.Context (Context(..)) where

import GHC.Conc (TVar)

import Server.Actions (Actions)

import ServerState (ServerState)
import ServerState.Bot (Bot)
import ServerState.User (User)

data Context = Context
    { state :: TVar ServerState
    , bot :: Bot
    , botUser :: User
    , actions :: Actions
    }
