-- TODO: this is copy-paste from tests with little changes.

{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Blockchain.Language.Extra where

import Enecuum.Prelude

import qualified Enecuum.Language                as L
import qualified Enecuum.Blockchain.Domain.Graph as G
import qualified Enecuum.Core.Types as D
import qualified Enecuum.Framework.Domain as D

class HasGraph s a | s -> a where
  graph :: Lens' s a


withGraph
  :: HasGraph s G.GraphVar
  => s
  -> G.GraphL a
  -> L.StateL a
withGraph s = L.evalGraph (s ^. graph)

withGraphIO
  :: HasGraph s G.GraphVar
  => s
  -> G.GraphL a
  -> L.NodeL a
withGraphIO s = L.evalGraphIO (s ^. graph)

makeRpcRequest
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeL (Either Text b)
makeRpcRequest connectCfg arg = L.evalNetworking $ L.makeRpcRequest_ connectCfg arg

makeRpcRequest'
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeL (Either Text b)
makeRpcRequest' connectCfg arg = L.evalNetworking $ L.makeRpcRequest' connectCfg arg

makeRequestUnsafe
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeL b
makeRequestUnsafe connectCfg arg =
    (\(Right a) -> a) <$> makeRpcRequest connectCfg arg

makeRequestUnsafe'
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeL b
makeRequestUnsafe' connectCfg arg =
    (\(Right a) -> a) <$> makeRpcRequest' connectCfg arg

