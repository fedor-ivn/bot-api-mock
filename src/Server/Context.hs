module Server.Context (Context (Context, state, token)) where

import GHC.Conc (TVar)
import Server.Token (Token)
import State (State)

data Context = Context
  { state :: TVar State,
    token :: Token
  }
