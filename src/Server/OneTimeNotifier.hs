module Server.OneTimeNotifier (OneTimeNotifier, new, wait, notify) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)

-- | Notify another thread about a performed action.
newtype OneTimeNotifier = OneTimeNotifier (MVar ())

-- | Create a new `OneTimeNotifier`.
new :: IO OneTimeNotifier
new = OneTimeNotifier <$> newEmptyMVar

-- | Block until some action is performed.
wait :: OneTimeNotifier -> IO ()
wait (OneTimeNotifier mVar) = readMVar mVar

-- | Notify that some action was performed.
notify :: OneTimeNotifier -> IO ()
notify (OneTimeNotifier mVar) = putMVar mVar ()
