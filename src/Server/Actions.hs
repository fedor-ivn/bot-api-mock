module Server.Actions (Actions, Action (..), ActionKind (..), writeAction) where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad.Cont (MonadIO, liftIO)
import qualified Server.Token as Token
import ServerState.Id (Id)

data ActionKind = GetMe | SendMessage

data Action = Action Id ActionKind

type Actions = Chan Action

writeAction ::
  MonadIO m =>
  Token.Token ->
  Chan Action ->
  ActionKind ->
  m ()
writeAction token actions kind = do
  let actor = Token.getId token
  let action = Action actor kind
  liftIO $ writeChan actions action
