module Enecuum.Assets.Services.Pos.Cryptography
    ( signMicroBlock
    ) where

import           Enecuum.Prelude
import qualified Enecuum.Domain                                             as D
import qualified Enecuum.Language                                           as L

import           Enecuum.Assets.Services.Pos.RuntimeData
import           Enecuum.Assets.Services.Pos.Messages

signMicroBlock :: PosServiceRuntimeData -> UnsignedMicroblock -> L.NodeL (Maybe D.Microblock)
signMicroBlock _ (UnsignedMicroblock _ _ mBlock) = pure $ Just mBlock