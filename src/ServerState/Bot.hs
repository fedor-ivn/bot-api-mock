{-# OPTIONS_GHC -Wno-missing-fields #-}

module ServerState.Bot (Bot (..), addUpdate) where

import Data.Maybe (fromJust, listToMaybe)
import Data.Sequence (Seq (Empty), ViewL (EmptyL, (:<)), viewl, (<|))
import qualified Data.Sequence as Seq
import Server.Token (Token)
import ServerState.BotPermissions (BotPermissions)
import ServerState.Id (Id)
import qualified ServerState.Id as Id
import ServerState.Message (Message)
import ServerState.Update (Update (Update))
import qualified ServerState.Update as Update

-- | Additional information about a bot.
data Bot = Bot
  { token :: Token,
    permissions :: BotPermissions,
    updates :: Seq Update
  }

-- | We push elements to the left end
-- | and get element from the right end
type Updates = Seq Update

getLeft :: ViewL Update -> Id
getLeft EmptyL = Id.Id 0
getLeft (x :< xs) = succ $ Update.id x

nextUpdateId :: Updates -> Id
nextUpdateId upds = getLeft $ viewl upds

addUpdate :: Maybe Bot -> Message -> Maybe Bot
addUpdate Nothing _ = Nothing
addUpdate (Just bot) message = Just $ bot {updates = newUpdate <| upds}
  where
    upds = updates bot
    newUpdate = Update (nextUpdateId upds) message
