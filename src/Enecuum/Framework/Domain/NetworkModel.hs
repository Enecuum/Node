{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Domain.NetworkModel where

import           Enecuum.Prelude

-- This is a raw vision of networking model.
-- There is a probability we don't need this model at all.

data NetworkConfig = NetworkConfig

data DNSQuery = DNSQuery
data DNSResponse = DNSResponse

-- TODO: needs more type safety to prevent passing MulticastRequest to broadcast (and vice versa).
-- Use TF

-- These types are just dummy.

data NetworkRequest = MulticastRequest
  { dummy :: Text }

data NetworkResponse = NetworkResponse
  { dummy :: Text }

-- | Declares how to convert to and from low-level network requests and responses.
-- This is a draft.
class NetworkMethod cfg req resp | req -> resp, resp -> req where
    toNetworkRequest :: cfg -> req -> NetworkRequest
    fromNetworkResponse :: cfg -> NetworkResponse -> Maybe resp


-- TODO: replace by Interval
type WaitingTimeout = Float
