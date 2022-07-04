module ServerState.PrivateChat.Id (PrivateChatId, make, getPeers) where

import ServerState.User.Id (UserId)

-- | An ID of a private chat between two users (peers).
--
-- This type has an invariant: the IDs must be ordered so that the ID stays the
-- same no matter in which order the peers' IDs were provided. For this reason,
-- a value of this type can only be constructed via `make`.
data PrivateChatId = PrivateChatId UserId UserId
    deriving (Eq, Ord, Show)

-- | Construct a `PrivateChatId`.
make :: UserId -> UserId -> PrivateChatId
make from to
    | to < from = PrivateChatId to from
    | otherwise = PrivateChatId from to

-- | Get peers of a private chat with this ID.
--
-- The order of returned IDs is not guaranteed.
getPeers :: PrivateChatId -> (UserId, UserId)
getPeers (PrivateChatId from to) = (from, to)
