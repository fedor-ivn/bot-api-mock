{-# LANGUAGE OverloadedStrings #-}

module Api.Ping (ping, Ping) where

import Data.Aeson (ToJSON (toJSON))
import Server.Context (Context)
import Server.Response (Response (Ok))

data Ping = Pong

instance ToJSON Ping where
  toJSON Pong = "pong"

ping :: Context -> Response Ping
ping _ = Ok Pong
