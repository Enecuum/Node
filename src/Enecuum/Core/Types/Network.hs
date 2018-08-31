{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Core.Types.Network where

import           Data.Text                                ( Text )

-- This is a raw vision of networking model. Will be updated later.

data NetworkCfg = NetworkCfg

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

