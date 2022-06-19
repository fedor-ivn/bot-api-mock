module Server.Context (Context (Context, state, token)) where

import GHC.Conc (TVar)
import Server.Token (Token)
import ServerState (ServerState)

data Context = Context
  { state :: TVar ServerState,
    token :: Token
  }
