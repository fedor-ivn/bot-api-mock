{-# LANGUAGE NamedFieldPuns #-}

module ServerState.Bot (Bot(..), addUpdate) where

import Data.Sequence (Seq, (|>))

import Server.Token (Token)

import ServerState.BotPermissions (BotPermissions)
import ServerState.CompleteMessage (CompleteMessage)
import ServerState.Update (Update(Update))
import ServerState.Update.Id (UpdateId)

-- | We push elements to the right end
-- and get element from the left end
type Updates = Seq Update

-- | Additional information about a bot.
data Bot = Bot
    { token :: Token
    , permissions :: BotPermissions
    , updates :: Updates
    , nextUpdateId :: UpdateId
    }

-- | Add a new update for the bot.
addUpdate :: CompleteMessage -> Bot -> Bot
addUpdate message bot@Bot { updates, nextUpdateId } = bot
    { updates = updates |> Update nextUpdateId message
    , nextUpdateId = succ nextUpdateId
    }
