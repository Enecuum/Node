{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Core.Types.Control where

import           Enecuum.Prelude

-- | Control: MVar Reqeust-Response pattern (STM version).
data Control req resp = Control
    { _request  :: MVar req   -- ^ ControlRequest  channel.
    , _response :: MVar resp  -- ^ ControlResponse channel.
    }

