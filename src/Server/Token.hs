{-# LANGUAGE OverloadedStrings #-}

module Server.Token (Token (Token)) where

import Data.Text (Text)
import qualified Data.Text as Text
import Servant (FromHttpApiData (parseQueryParam, parseUrlPiece))

newtype Token = Token Text deriving (Eq)

instance FromHttpApiData Token where
  parseUrlPiece piece = case Text.stripPrefix "bot" piece of
    Nothing -> Left "Failed to parse bot token"
    Just token -> Right (Token token)
