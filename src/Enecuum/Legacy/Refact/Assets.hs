module Enecuum.Legacy.Refact.Assets where

import Enecuum.Prelude

import Data.String (IsString)

import Enecuum.Legacy.Service.Types ( KeyBlockInfoPoW (..) )

genesisIndicationHash :: IsString a => a
genesisIndicationHash = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

genesisSolver :: IsString a => a
genesisSolver = "EMde81cgGToGrGWSNCqm6Y498qBpjEzRczBbvC5MV2Q="

genesisKeyBlock :: KeyBlockInfoPoW
genesisKeyBlock = KeyBlockInfoPoW
  { _time      = 0
  , _prev_hash = genesisIndicationHash
  , _number    = 0
  , _nonce     = 0
  , _solver    = genesisSolver
  , _type      = 0
  }
