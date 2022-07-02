module Server.Internal (Server(..)) where

import GHC.Conc (TVar)
import Server.Actions (Actions)
import ServerState (ServerState)

-- | A mock Bot API server.
data Server = Server
    { stateVar :: TVar ServerState
    , actions :: Actions
    }
