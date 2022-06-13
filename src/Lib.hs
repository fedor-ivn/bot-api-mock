module Lib (startServer) where

import Network.Wai.Handler.Warp (run)
import Server (app)

startServer :: IO ()
startServer = app >>= run 8081
