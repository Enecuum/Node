{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.Routing.Runtime where

import qualified Data.Aeson                       as J
import qualified Data.Map                         as Map
import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Config
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasStatus)
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Assets.Nodes.Routing.Messages

type BnAddress = NodeAddress

data RoutingRuntime = RoutingRuntime
    { _hostAddress :: D.StateVar (Maybe D.Host) 
    , _nodePorts   :: NodePorts
    , _nodeId      :: NodeId
    , _bnAddress   :: NodeAddress
    , _connectMap  :: D.StateVar (ChordRouteMap NodeAddress)
    }
makeFieldsNoPrefix ''RoutingRuntime

makeRoutingRuntimeData :: NodePorts -> NodeId -> BnAddress -> L.NodeL RoutingRuntime
makeRoutingRuntimeData nodePorts' nodeId' bnAdress' = do
    myHostAddress <- L.newVarIO Nothing
    myConnectMap  <- L.newVarIO mempty
    pure $ RoutingRuntime myHostAddress nodePorts' nodeId' bnAdress' myConnectMap

getMyNodeAddress :: RoutingRuntime -> L.NodeL (Maybe NodeAddress)
getMyNodeAddress routingRuntime = do
    mHost <- L.readVarIO (routingRuntime ^. hostAddress)
    case mHost of
        Just host -> pure $ Just $
            NodeAddress host (routingRuntime ^. nodePorts) (routingRuntime ^. nodeId)
        Nothing   -> pure Nothing
--
periodic time action = L.process $ forever $ do
    L.delay time
    action

getConnects :: RoutingRuntime -> L.NodeL (ChordRouteMap NodeAddress)
getConnects routingRuntime = L.readVarIO (routingRuntime ^. connectMap)

clearingOfConnects :: RoutingRuntime -> L.NodeL ()
clearingOfConnects routingRuntime = do
    connects <- getConnects routingRuntime
    L.atomically $ do
        let myNodeId        = routingRuntime ^. nodeId
        let filteredNodes   = maybeToList (findNextForHash myNodeId connects) <> findInMap myNodeId connects
        let filteredNodeMap = toChordRouteMap filteredNodes
        L.writeVar (routingRuntime ^. connectMap) filteredNodeMap

successorsRequest :: RoutingRuntime -> L.NodeL ()
successorsRequest routingRuntime = do
    myAddress <- getMyNodeAddress routingRuntime
    whenJust myAddress $ \myNodeAddress -> do
        connectMap <- getConnects routingRuntime
        forM_ connectMap $ \(_, addr) ->
            void $ L.notify (getUdpAddress addr) $ NextForYou $ getUdpAddress myNodeAddress


pingConnects :: ChordRouteMap NodeAddress -> L.NodeL [NodeId]
pingConnects nodes = do
    deadNodes <- forM (elems nodes) $ \(nodeId, address) -> do
        res <- L.makeRpcRequest (getRpcAddress address) M.Ping
        pure $ case res of
            Right M.Pong -> Nothing
            Left  _      -> Just nodeId
    pure $ catMaybes deadNodes

routingWorker :: RoutingRuntime -> L.NodeDefinitionL ()
routingWorker routingRuntime = do
    let bnUdpAddress = getUdpAddress (routingRuntime^.bnAddress)

    builtIntoTheNetwork routingRuntime
    periodic (1000 * 1000)  $ clearingOfConnects routingRuntime
    periodic (1000 * 10000) $ successorsRequest  routingRuntime
    periodic (1000 * 1000)  $ sendHelloToPrevius routingRuntime
    periodic (1000 * 1000)  $ do
        deadNodes <- pingConnects =<< getConnects routingRuntime
        forM_ deadNodes $ \hash -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ removeFromMap hash
            L.notify bnUdpAddress $ M.IsDead hash

builtIntoTheNetwork :: RoutingRuntime -> L.NodeDefinitionL ()
builtIntoTheNetwork routingRuntime = do
    registerWithBn routingRuntime
    forM_ [connecRequests 63, nextRequest, sendHelloToPrevius]
        $ \action -> L.scenario $ action routingRuntime

registerWithBn :: RoutingRuntime -> L.NodeDefinitionL ()
registerWithBn routingRuntime = do
    let bnUdpAddress =  getUdpAddress (routingRuntime^.bnAddress)
    let privateKey   =  True
    helloToBn        <- makeHelloToBn privateKey (routingRuntime^.nodePorts) (routingRuntime^.nodeId)
    let takeAddress  =  do
            L.notify bnUdpAddress helloToBn
            L.delay 1000000
            unlessM (isJust <$> L.readVarIO (routingRuntime ^. hostAddress)) takeAddress
    L.process takeAddress
    void $ L.await (routingRuntime^.hostAddress)

connecRequests :: Word64 -> RoutingRuntime -> L.NodeL ()
connecRequests i routingRuntime = when (i > 0) $ do
    let bnRpcAddress =  getRpcAddress (routingRuntime^.bnAddress)
    let myNodeId     =  routingRuntime ^. nodeId
    maybeAddress     <- L.makeRpcRequest bnRpcAddress $ M.ConnectRequest myNodeId i
    myAddress        <- getMyNodeAddress routingRuntime
    case maybeAddress of
        Right (recivedNodeId, address) | myAddress /= Just address -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ addToMap recivedNodeId address
            connecRequests (i - 1) routingRuntime
        _ -> pure ()

nextRequest :: RoutingRuntime -> L.NodeL ()
nextRequest routingRuntime = do
    let bnRpcAddress  =  getRpcAddress (routingRuntime^.bnAddress)
    let myNodeId      =  routingRuntime ^. nodeId
    myAddress        <- getMyNodeAddress routingRuntime
    nextForMe        <- L.makeRpcRequest bnRpcAddress $ M.NextForMe myNodeId
    case nextForMe of
        Right (recivedNodeId, address) | myAddress /= Just address ->
            L.modifyVarIO (routingRuntime ^. connectMap) (addToMap recivedNodeId address)
        _ -> pure ()

sendHelloToPrevius :: RoutingRuntime -> L.NodeL () 
sendHelloToPrevius routingRuntime = do
    connectMap      <- getConnects      routingRuntime
    myNodeAddress   <- getMyNodeAddress routingRuntime
    let myNodeId    =  routingRuntime ^. nodeId
    let mAddress    =  findNextForHash myNodeId connectMap
    case (myNodeAddress, mAddress) of
        (Just myAddress, Just (_, reciverAddress)) -> do  
            let privateKey  = True
            hello <- makeRoutingHello privateKey myAddress
            void $ L.notify (getUdpAddress reciverAddress) hello
        _ -> pure ()

--

udpRoutingHandlers routingRuntime = do
    L.handler $ acceptHello             routingRuntime
    L.handler $ acceptConnectResponse   routingRuntime
    L.handler $ acceptNextForYou        routingRuntime
    L.handler $ acceptSendTo            routingRuntime

rpcRotingHandlers routingRuntime = L.method rpcPingPong


acceptHello :: RoutingRuntime -> RoutingHello -> D.Connection D.Udp -> L.NodeL ()
acceptHello routingRuntime routingHello con = do
    L.close con
    let senderAddress :: NodeAddress
        senderAddress  = routingHello ^. nodeAddress
    let senderId       = senderAddress ^. nodeId
    let myId           = routingRuntime ^. nodeId
    when (verifyRoutingHello routingHello) $ do
        connects <- getConnects routingRuntime
        let nextAddres = nextForHello myId senderId connects
        
        whenJust nextAddres $ \reciverAddress ->
            void $ L.notify (getUdpAddress reciverAddress) routingHello
        
        L.modifyVarIO
            (routingRuntime ^. connectMap)
            (addToMap senderId senderAddress)
    

acceptNextForYou :: RoutingRuntime -> NextForYou -> D.Connection D.Udp -> L.NodeL ()
acceptNextForYou routingRuntime (NextForYou senderAddress) conn = do
    L.close conn
    connectMap <- getConnects routingRuntime
    maybeAddress <- L.atomically $ do
        let myId   =  routingRuntime ^. nodeId
        pure $ findNextForHash myId connectMap
    whenJust maybeAddress $ \(nodeId, address) ->
        void $ L.notify senderAddress address

acceptConnectResponse :: RoutingRuntime -> NodeAddress -> D.Connection D.Udp -> L.NodeL ()
acceptConnectResponse routingRuntime address con = do
    mAddress <- getMyNodeAddress routingRuntime
    whenJust mAddress $ \myAddress -> do 
        let nId = address^.nodeId
        when (myAddress /= address) $
            L.modifyVarIO (routingRuntime ^. connectMap) (addToMap nId address)
    L.close con

