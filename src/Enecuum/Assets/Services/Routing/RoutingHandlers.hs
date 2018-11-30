module Enecuum.Assets.Services.Routing.RoutingHandlers where

import qualified Enecuum.Assets.Nodes.Address             as A
import qualified Enecuum.Assets.Nodes.Messages            as M
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Services.Routing.Messages
import           Enecuum.Assets.Services.Routing.RuntimeData
import qualified Enecuum.Domain                           as D
import qualified Enecuum.Framework.Lens                   as Lens
import qualified Enecuum.Language                         as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap

udpRoutingHandlers :: RoutingRuntime -> L.NetworkHandlerL D.Udp L.NodeL ()
udpRoutingHandlers routingRuntime = do
    L.handler $ acceptHello             routingRuntime
    L.handler $ acceptConnectResponse   routingRuntime
    L.handler $ acceptNextForYou        routingRuntime

rpcRoutingHandlers :: RoutingRuntime -> L.RpcHandlerL L.NodeL ()
rpcRoutingHandlers routingRuntime = do
    L.method    rpcPingPong
    L.methodE $ findConnect routingRuntime
    L.method  $ connectMapRequest routingRuntime

-- answer to the questioner who is successor for me
acceptNextForYou :: RoutingRuntime -> NextForYou -> D.Connection D.Udp -> L.NodeL ()
acceptNextForYou routingRuntime (NextForYou senderAddress) conn = do
    L.close conn
    connects <- getConnects routingRuntime
    let mAddress = snd <$> findNextForHash (getMyNodeId routingRuntime) connects
    whenJust mAddress $ \address -> void $ L.notify senderAddress address

--
findConnect :: RoutingRuntime -> M.ConnectRequest -> L.NodeL (Either Text D.NodeAddress)
findConnect routingRuntime (M.ConnectRequest hash i) = do
    connects <- getConnects routingRuntime
    let address = snd <$> findInMapNByKey (\h j -> D.hashToWord64 h + 2 ^ j) i hash connects
    pure $ maybe (Left "Connection map is empty.") Right address

-- | Processing of messages forwarded to maintain the integrity of the network structure
--   clarifying the predecessor and successor relationship
acceptHello :: RoutingRuntime -> RoutingHello -> D.Connection D.Udp -> L.NodeL ()
acceptHello routingRuntime routingHello con = do
    L.close con
    when (verifyRoutingHello routingHello) $ do
        connects <- getConnects routingRuntime
        let senderAddress = routingHello ^. nodeAddress
        let senderNodeId  = senderAddress ^. Lens.nodeId
        let nextAddres    = nextForHello (getMyNodeId routingRuntime) senderNodeId connects
        whenJust nextAddres $ \receiverAddress ->
            void $ L.notify (A.getUdpAddress receiverAddress) routingHello

        L.modifyVarIO
            (routingRuntime ^. connectMap)
            (addToMap senderNodeId senderAddress)

acceptConnectResponse :: RoutingRuntime -> D.NodeAddress -> D.Connection D.Udp -> L.NodeL ()
acceptConnectResponse routingRuntime address con = do
    L.close con
    -- if this address is not mine, then add it
    unless (address == routingRuntime ^. myNodeAddres) $
        L.modifyVarIO (routingRuntime ^. connectMap) (addToMap (address ^. Lens.nodeId) address)

connectMapRequest :: RoutingRuntime -> M.ConnectMapRequest -> L.NodeL [D.NodeAddress]
connectMapRequest nodeRuntime _ =
    -- return all known connections
    (snd <$>) . fromChordRouteMap <$> L.readVarIO (nodeRuntime ^. connectMap)
