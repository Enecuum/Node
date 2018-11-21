module Enecuum.Assets.Nodes.Routing.RoutingHandlers where

import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Domain                   as D
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Assets.Nodes.Routing.Messages
import           Enecuum.Assets.Nodes.Routing.RuntimeData

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
    let mAddress = snd <$> findNextForHash (routingRuntime ^. myNodeAddres . A.nodeId) connects
    whenJust mAddress $ \address -> void $ L.notify senderAddress address

--
findConnect :: RoutingRuntime -> M.ConnectRequest -> L.NodeL (Either Text A.NodeAddress)
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
        let senderNodeId  = senderAddress ^. A.nodeId
        let nextAddres    = nextForHello (routingRuntime ^. myNodeAddres . A.nodeId) senderNodeId connects
        whenJust nextAddres $ \reciverAddress ->
            void $ L.notify (A.getUdpAddress reciverAddress) routingHello
        
        L.modifyVarIO
            (routingRuntime ^. connectMap)
            (addToMap senderNodeId senderAddress)

acceptConnectResponse :: RoutingRuntime -> A.NodeAddress -> D.Connection D.Udp -> L.NodeL ()
acceptConnectResponse routingRuntime address con = do
    L.close con
    -- if this address is not mine, then add it
    unless (address == routingRuntime ^. myNodeAddres) $
        L.modifyVarIO (routingRuntime ^. connectMap) (addToMap (address ^. A.nodeId) address)

connectMapRequest :: RoutingRuntime -> M.ConnectMapRequest -> L.NodeL [A.NodeAddress]
connectMapRequest nodeRuntime _ =
    -- return all known connections
    (snd <$>) . fromChordRouteMap <$> L.readVarIO (nodeRuntime ^. connectMap)
