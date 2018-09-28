{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Language.Extra where

import Enecuum.Prelude

import qualified Enecuum.Framework.State.Language      as L
import qualified Enecuum.Framework.Node.Language       as L
import qualified Enecuum.Framework.Networking.Language as L
import qualified Enecuum.Core.Language                 as L
import qualified Enecuum.Core.Types                    as D
import qualified Enecuum.Framework.Domain              as D
import           Data.HGraph.StringHashable            (StringHashable)
import           Data.HGraph.THGraph                   (THGraph)

-- | Allows to extract graph variable from any structure.
-- To use this type class, you need to export it unqualified in scope of your data type lenses:
-- import Enecuum.Language (HasGraph)
class HasGraph s a | s -> a where
  graph :: Lens' s a

-- | Evals some graph action (atomically) having a structure that contains a graph variable.
withGraph
  :: HasGraph s (TVar (THGraph d))
  => (Serialize d, StringHashable d)
  => s
  -> L.HGraphL d a
  -> L.StateL a
withGraph s = L.evalGraph (s ^. graph)

-- | Evals some graph action (non-atomically) having a structure that contains a graph variable.
withGraphIO
  :: HasGraph s (TVar (THGraph d))
  => (Serialize d, StringHashable d)
  => s
  -> L.HGraphL d a
  -> L.NodeL a
withGraphIO s = L.evalGraphIO (s ^. graph)

-- TODO: make this a type class?
-- | Makes RPC call.
makeRpcRequest
    :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> L.NodeL (Either Text b)
makeRpcRequest connectCfg arg = L.evalNetworking $ L.makeRpcRequest' connectCfg arg

-- | Makes unsafe RPC call. Not recommended to use.
makeRpcRequestUnsafe
    :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> L.NodeL b
makeRpcRequestUnsafe connectCfg arg = makeRpcRequest connectCfg arg >>= \case
    Left err -> error err
    Right a  -> pure a
