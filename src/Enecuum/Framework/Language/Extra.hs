{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Language.Extra where

import           Enecuum.Prelude

import           Data.HGraph.StringHashable                (StringHashable)
import qualified Enecuum.Core.Language                     as L
import qualified Enecuum.Core.Types                        as D
import qualified Enecuum.Framework.Domain                  as D
import qualified Enecuum.Framework.Networking.Language     as L
import qualified Enecuum.Framework.Node.Language           as L
import qualified Enecuum.Framework.NodeDefinition.Language as L

-- TODO: move all possible functions to Core.Language.Extra.

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
class HasStatus s a | s -> a where
    status :: Lens' s a

-- | Evals some graph action (atomically) having a structure that contains a graph variable.
-- To use it, you need to export HasGraph type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasGraph)

withGraph
    :: (HasGraph s (D.TGraph c), Serialize c, StringHashable c)
    => s
    -> Free (L.HGraphF (D.TNodeL c)) a
    -> L.StateL a
withGraph s = L.evalGraph (s ^. graph)

-- | Evals some graph action (non-atomically) having a structure that contains a graph variable.
-- To use it, you need to export HasGraph type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasGraph)

withGraphIO
    :: (HasGraph s (D.TGraph c), Serialize c, StringHashable c)
    => s
    -> Free (L.HGraphF (D.TNodeL c)) a
    -> L.NodeL a
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

stopNode :: HasStatus s (D.StateVar D.NodeStatus) => s -> L.NodeL ()
stopNode nodeData   = stopNode' (nodeData ^. status)

stopNode' :: D.StateVar D.NodeStatus -> L.NodeL ()
stopNode' statusVar = L.atomically $ L.writeVar statusVar D.NodeFinished

-- | Forces node to stop (actually just fills the `status` field in the data structure. Use `nodeFinishPending` to await `status`.)
-- To use it, you need to export HasStatus type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasStatus)
stopNodeHandler :: HasStatus s (D.StateVar D.NodeStatus) => s -> D.StopNode -> L.NodeL Text
stopNodeHandler nodeData     = stopNodeHandler' (nodeData ^. status)

-- | Forces node to stop (actually just fills the variable)
stopNodeHandler' :: D.StateVar D.NodeStatus -> D.StopNode -> L.NodeL Text
stopNodeHandler' statusVar _ = stopNode' statusVar >> pure "Finished."

-- | Makes node awaiting for finishing.
-- To use it, you need to export HasStatus type class unqualified to the scope of your data type lenses
-- (made by `makeFieldsNoPrefix`):
-- import Enecuum.Language (HasStatus)
awaitNodeFinished :: HasStatus s (D.StateVar D.NodeStatus) => s -> L.NodeDefinitionL ()
awaitNodeFinished nodeData = L.scenario $ L.atomically $ unlessM isNodeFinished L.retry
    where
        isNodeFinished = do
            s <- L.readVar $ nodeData ^. status
            pure $ s == D.NodeFinished

-- | Makes node awaiting for finishing.
awaitNodeFinished'
    :: D.StateVar D.NodeStatus
    -> L.NodeDefinitionL ()
awaitNodeFinished' statusVar = L.scenario $ L.atomically $ unlessM isNodeFinished L.retry
    where
        isNodeFinished = do
            s <- L.readVar statusVar
            pure $ s == D.NodeFinished

-- | Makes node awaiting forever.
awaitNodeForever :: L.NodeDefinitionL ()
awaitNodeForever = L.scenario $ L.atomically $ do
    -- This is a little trick to make STM thinking that something can change.
    -- So it won't stop with message:
    -- 'thread blocked indefinitely in an STM transaction'
    xVar <- L.newVar (1 :: Int)
    x <- L.readVar xVar
    when (x == 1) L.retry

-- | Resumes when some external process changes signalVar to True.
-- Returns this var to False immediately.
awaitSignal :: (Monad m, L.StateIO m) => D.StateVar Bool -> m ()
awaitSignal signalVar = do
    L.atomically $ unlessM (L.readVar signalVar) L.retry
    L.writeVarIO signalVar False

takeVar :: D.StateVar (Maybe a) -> L.StateL a
takeVar var = do
    mbVal <- L.readVar var
    case mbVal of
        Just val -> do
            L.writeVar var Nothing
            pure val
        Nothing  -> L.retry

modifyVarIO :: L.StateIO m => D.StateVar a -> (a -> a) -> m ()
modifyVarIO var f = L.atomically $ L.modifyVar var f
