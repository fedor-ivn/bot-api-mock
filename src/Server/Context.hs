module Server.Context (Context(Context, actions, state, token)) where

import GHC.Conc (TVar)

import Server.Actions (Actions)
import Server.Token (Token)

import ServerState (ServerState)

data Context = Context
    { state :: TVar ServerState
    , token :: Token
    , actions :: Actions
    }
