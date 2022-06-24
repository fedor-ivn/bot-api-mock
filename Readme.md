# Telegram Bot API mock server

This library implements a mock Bot API server. Its primary use is to test a Telegram bot.
Here's an example:

```hs
-- | An initial state for the server.
initialState :: InitialState

-- | Settings for the server, e.g. its port and host.
serverSettings :: Warp.Settings

main :: IO ()
main = startServer initialState serverSettings $ \server -> do
  -- ...start your bot however you like

  currentTime <- Time <$> getCurrentTime
  modifyState server $ do
    ServerState.sendMessage userId botId currentTime "/start"

  waitForAction botId SendMessage server
  putStrLn "The bot has reacted to /start. Yay!"
  return ()
```

A more detailed example is available in the [`example`](./example/) directory. This directory
contains its own readme file with instructions on how to run that example.

## Development

This project uses Stack for development. We use GHC of version 8.10.7.
