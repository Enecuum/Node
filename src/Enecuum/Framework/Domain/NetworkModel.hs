{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Domain.NetworkModel where

import           Enecuum.Prelude

-- This is a raw vision of networking model. Will be updated later.

data NetworkConfig = NetworkConfig

data DNSQuery = DNSQuery
data DNSResponse = DNSResponse

-- TODO: needs more type safety to prevent passing MulticastRequest to broadcast (and vice versa).
-- Use TF
data NetworkRequest = MulticastRequest
  { dummy :: Text

  }

data NetworkResponse = NetworkResponse
  { dummy :: Text
  }

class NetworkMethod cfg req resp | req -> resp, resp -> req where
    toNetworkRequest :: cfg -> req -> NetworkRequest
    fromNetworkResponse :: cfg -> NetworkResponse -> Maybe resp


-- TODO: replace by Interval
type WaitingTimeout = Float
