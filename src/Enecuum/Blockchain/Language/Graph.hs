module Enecuum.Blockchain.Language.Graph where

import Enecuum.Prelude
import qualified Enecuum.Framework.Language as L
import qualified Enecuum.Core.Language as L
import qualified Enecuum.Domain as D

import qualified Enecuum.Framework.LogState as Log
import qualified Enecuum.Blockchain.Lens as Lens

-- | Get kBlock by Hash
getKBlock :: D.StateVar [Text] -> D.BlockchainData -> D.StringHash -> L.StateL (Maybe D.KBlock)
getKBlock logV bData hash = do
    (res, mbMsg) <- L.withGraph bData $ do
        maybeKBlock <- L.getNode hash
        case maybeKBlock of
            Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _) -> pure $ (Just kBlock, Nothing)
            _ -> pure (Nothing, Just $ "KBlock not found by hash: " <> show hash)
    whenJust mbMsg $ Log.stateLog logV
    pure res