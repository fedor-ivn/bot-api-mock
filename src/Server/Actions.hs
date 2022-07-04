module Server.Actions (Actions, Action(..), ActionKind(..), writeAction) where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad.Cont (MonadIO, liftIO)

import ServerState.User.Id (UserId)

data ActionKind =
    GetMe
    | SendMessage
    | LogOut
    | Close
    | GetUpdates
    | DeleteWebhook
    deriving (Eq)

data Action = Action UserId ActionKind
    deriving Eq

type Actions = Chan Action

writeAction :: MonadIO m => UserId -> Actions -> ActionKind -> m ()
writeAction actor actions kind = liftIO (writeChan actions action)
    where action = Action actor kind
