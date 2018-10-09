{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Language.Extra where

import Enecuum.Prelude
import qualified Data.Aeson as A

import qualified Enecuum.Framework.State.Language      as L
import qualified Enecuum.Framework.Node.Language       as L
import qualified Enecuum.Framework.Networking.Language as L
import qualified Enecuum.Framework.NodeDefinition.Language as L
import qualified Enecuum.Core.Language                 as L
import qualified Enecuum.Core.Types                    as D
import qualified Enecuum.Framework.Domain              as D
import           Data.HGraph.StringHashable            (StringHashable)
import           Data.HGraph.THGraph                   (THGraph)

-- | Allows to extract graph variable from any structure.
-- To use it, you need to export it unqualified in scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasGraph)
class HasGraph s a | s -> a where
  graph :: Lens' s a

-- | Allows to extract `finished` variable from any structure.
-- To use it, you need to export it unqualified in scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasFinished)
class HasFinished s a | s -> a where
  finished :: Lens' s a

data NodeFinished = NodeFinished
  deriving (Show, Eq, Generic)

instance A.FromJSON NodeFinished where
    parseJSON _ = pure NodeFinished


-- | Evals some graph action (atomically) having a structure that contains a graph variable.
-- To use it, you need to export HasGraph type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasGraph)
{-
withGraph
  :: HasGraph s (TVar (THGraph d))
  => (Serialize d, StringHashable d)
  => s
  -> L.HGraphL d a
  -> L.StateL a
-}
withGraph s = L.evalGraph (s ^. graph)

-- | Evals some graph action (non-atomically) having a structure that contains a graph variable.
-- To use it, you need to export HasGraph type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasGraph)
{-}
withGraphIO
  :: HasGraph s (TVar (THGraph d))
  => (Serialize d, StringHashable d)
  => s
  -> L.HGraphL d a
  -> L.NodeL a
  -}
withGraphIO s = L.evalGraphIO (s ^. graph)

-- TODO: make this a type class?
-- | Makes RPC call.
makeRpcRequest :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> L.NodeL (Either Text b)
makeRpcRequest connectCfg arg = L.evalNetworking $ L.makeRpcRequest' connectCfg arg

-- | Makes unsafe RPC call. Not recommended to use.
makeRpcRequestUnsafe :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> L.NodeL b
makeRpcRequestUnsafe connectCfg arg = makeRpcRequest connectCfg arg >>= \case
    Left  err -> error err
    Right a   -> pure a


-- | Forces node to stop (actually just fills the `finished` field in the data structure. Use `nodeFinishPending` to await `finished`.)
-- To use it, you need to export HasFinished type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasFinished)
setNodeFinished :: HasFinished s (D.StateVar Bool) => s -> NodeFinished -> L.NodeL Text
setNodeFinished nodeData NodeFinished = do
    L.atomically $ L.writeVar (nodeData ^. finished) True
    pure "Finished."

-- | Makes node awaiting for finishing.
-- To use it, you need to export HasFinished type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasFinished)
nodeFinishPending :: HasFinished s (D.StateVar Bool) => s -> L.NodeDefinitionL ()
nodeFinishPending nodeData = L.scenario $ L.atomically $ unlessM (L.readVar $ nodeData ^. finished) L.retry
