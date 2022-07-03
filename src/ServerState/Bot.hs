{-# OPTIONS_GHC -Wno-missing-fields #-}

module ServerState.Bot (Bot(..), addUpdate) where

import Data.Sequence (Seq, (|>))

import Server.Token (Token)

import ServerState.BotPermissions (BotPermissions)
import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Id (Id)
import ServerState.Update (Update(Update))

-- | We push elements to the right end
-- and get element from the left end
type Updates = Seq Update

-- | Additional information about a bot.
data Bot = Bot
    { token :: Token
    , permissions :: BotPermissions
    , updates :: Updates
    , updateId :: Id
    }

-- | Add a new update for the bot
addUpdate :: Maybe Bot -> CompleteMessage -> Maybe Bot
addUpdate Nothing _ = Nothing
addUpdate (Just bot) message = Just
    $ bot { updates = upds |> newUpdate, updateId = nextUpdateId }
  where
    upds = updates bot
    nextUpdateId = succ (updateId bot)
    newUpdate = Update (updateId bot) message
