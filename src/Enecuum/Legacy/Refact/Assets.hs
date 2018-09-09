module Enecuum.Legacy.Refact.Assets where

import Enecuum.Legacy.Service.Types ( KeyBlockInfoPoW (..) )

genesisHash = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
solver = "EMde81cgGToGrGWSNCqm6Y498qBpjEzRczBbvC5MV2Q="

genesisKeyBlock :: KeyBlockInfoPoW
genesisKeyBlock = KeyBlockInfoPoW
  { _time = 0
  , _prev_hash = genesisHash
  , _number = 0
  , _nonce = 0
  , _solver = solver
  , _type = 0
  }
