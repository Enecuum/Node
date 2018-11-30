module Enecuum.Assets.Services.Pos.Cryptography
    ( signMicroBlock
    ) where

import           Enecuum.Prelude
import qualified Enecuum.Domain                                             as D
import qualified Enecuum.Language                                           as L

import           Enecuum.Assets.Services.Pos.RuntimeData

signMicroBlock :: PosServiceRuntimeData -> D.UnsignedMicroblock -> L.NodeL (Maybe D.Microblock)
signMicroBlock _ (D.UnsignedMicroblock mBlock) = pure $ Just mBlock