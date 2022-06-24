module Server.Actions (Actions, Action (..), ActionKind (..), writeAction) where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad.Cont (MonadIO, liftIO)
import Server.Token (Token)
import qualified Server.Token as Token
import ServerState.Id (Id)

data ActionKind = GetMe | SendMessage | LogOut | Close deriving (Eq)

data Action = Action Id ActionKind deriving (Eq)

type Actions = Chan Action

writeAction :: MonadIO m => Token -> Actions -> ActionKind -> m ()
writeAction token actions kind = liftIO (writeChan actions action)
  where
    actor = Token.getId token
    action = Action actor kind
