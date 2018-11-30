{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Services.Routing.RuntimeData where

import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import qualified Enecuum.Assets.Nodes.Address   as A
import           Enecuum.Domain                 (NodeAddress (..), NodeId (..), NodePorts (..))
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Framework.Lens         as Lens
import qualified Enecuum.Language               as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap

type BnAddress = NodeAddress

data RoutingRuntime = RoutingRuntime
    { _myNodeAddres :: NodeAddress
    , _bnAddress    :: NodeAddress
    , _connectMap   :: D.StateVar (ChordRouteMap NodeAddress)
    , _msgFilter    :: D.StateVar (Seq.Seq (Set.Set D.StringHash))
    }
makeFieldsNoPrefix ''RoutingRuntime

makeRoutingRuntimeData :: NodeAddress -> BnAddress -> L.NodeL RoutingRuntime
makeRoutingRuntimeData myNodeAddress' bnAdress' = do
    myConnectMap  <- L.newVarIO mempty
    msgFilterSets <- L.newVarIO $ Seq.fromList [Set.empty, Set.empty, Set.empty]
    pure $ RoutingRuntime myNodeAddress' bnAdress' myConnectMap msgFilterSets

class GetMyNodeId a where
    getMyNodeId :: a -> D.NodeId

instance GetMyNodeId RoutingRuntime where
    getMyNodeId routingRuntime = routingRuntime ^. myNodeAddres . Lens.nodeId

getConnects :: RoutingRuntime -> L.NodeL (ChordRouteMap NodeAddress)
getConnects routingRuntime = L.readVarIO (routingRuntime ^. connectMap)

-- | Add the message to list of familiar message.
registerMsg :: RoutingRuntime -> D.StringHash -> L.StateL ()
registerMsg routingRuntime messageHash =
    L.modifyVar (routingRuntime ^. msgFilter) (Seq.adjust (Set.insert messageHash) 0)

-- | Check if the message is familiar?
isInFilter :: RoutingRuntime -> D.StringHash -> L.StateL Bool
isInFilter routingRuntime messageHash = do
    sets <- L.readVar $ routingRuntime ^. msgFilter
    pure $ any (\i -> messageHash `Set.member` Seq.index sets i) [0..2]

