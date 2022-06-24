module Server.Context (Context (..)) where

import GHC.Conc (TVar)
import Server.Token (Token)
import ServerState (ServerState)

data Context = Context
  { state :: TVar ServerState,
    token :: Token
  }
