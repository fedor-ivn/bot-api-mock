module Server.Actions (Actions, Action(..), ActionKind(..), writeAction) where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad.Cont (MonadIO, liftIO)

import ServerState.Id (Id)

data ActionKind = GetMe | SendMessage | LogOut | Close | GetUpdates deriving (Eq)

data Action = Action Id ActionKind
    deriving Eq

type Actions = Chan Action

writeAction :: MonadIO m => Id -> Actions -> ActionKind -> m ()
writeAction actor actions kind = liftIO (writeChan actions action)
    where action = Action actor kind
