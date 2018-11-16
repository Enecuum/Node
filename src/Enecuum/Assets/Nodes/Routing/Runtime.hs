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
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set

type BnAddress = A.NodeAddress

data RoutingRuntime = RoutingRuntime
    { _hostAddress :: D.StateVar (Maybe D.Host) 
    , _nodePorts   :: A.NodePorts
    , _nodeId      :: A.NodeId
    , _bnAddress   :: A.NodeAddress
    , _connectMap  :: D.StateVar (ChordRouteMap A.NodeAddress)
    , _msgFilter   :: D.StateVar (Seq.Seq (Set.Set D.StringHash))
    }
makeFieldsNoPrefix ''RoutingRuntime

makeRoutingRuntimeData :: A.NodePorts -> A.NodeId -> BnAddress -> L.NodeL RoutingRuntime
makeRoutingRuntimeData nodePorts' nodeId' bnAdress' = do
    myHostAddress <- L.newVarIO Nothing
    myConnectMap  <- L.newVarIO mempty
    msgFilter     <- L.newVarIO $ Seq.fromList [Set.empty, Set.empty, Set.empty]
    pure $ RoutingRuntime myHostAddress nodePorts' nodeId' bnAdress' myConnectMap msgFilter

isInFilter routingRuntime messageHash = do
    sets <- L.readVar $ routingRuntime ^. msgFilter
    pure $ any (\i -> messageHash `Set.member` Seq.index sets i) [0..2]

registerMsg routingRuntime messageHash =
    L.modifyVar (routingRuntime ^. msgFilter) (Seq.adjust (Set.insert messageHash) 0)


getMyNodeAddress :: RoutingRuntime -> L.NodeL (Maybe A.NodeAddress)
getMyNodeAddress routingRuntime = do
    mHost <- L.readVarIO (routingRuntime ^. hostAddress)
    case mHost of
        Just host -> pure $ Just $
            A.NodeAddress host (routingRuntime ^. nodePorts) (routingRuntime ^. nodeId)
        Nothing   -> pure Nothing
--
periodic time action = L.process $ forever $ do
    L.delay time
    action

getConnects :: RoutingRuntime -> L.NodeL (ChordRouteMap A.NodeAddress)
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
            void $ L.notify (A.getUdpAddress addr) $ NextForYou $ A.getUdpAddress myNodeAddress


pingConnects :: ChordRouteMap A.NodeAddress -> L.NodeL [A.NodeId]
pingConnects nodes = do
    deadNodes <- forM (elems nodes) $ \(nodeId, address) -> do
        res <- L.makeRpcRequest (A.getRpcAddress address) M.Ping
        pure $ case res of
            Right M.Pong -> Nothing
            Left  _      -> Just nodeId
    pure $ catMaybes deadNodes

routingWorker :: RoutingRuntime -> L.NodeDefinitionL ()
routingWorker routingRuntime = do
    let bnUdpAddress = A.getUdpAddress (routingRuntime^.bnAddress)

    builtIntoTheNetwork routingRuntime
    periodic (1000 * 1000)  $ clearingOfConnects routingRuntime
    periodic (1000 * 10000) $ successorsRequest  routingRuntime
    periodic (1000 * 1000)  $ sendHelloToPrevius routingRuntime
    periodic (1000 * 1000)  $
        L.modifyVarIO (routingRuntime ^. msgFilter) $ 
            \seq -> Set.empty Seq.<| Seq.deleteAt 2 seq
    
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
    let bnUdpAddress =  A.getUdpAddress (routingRuntime^.bnAddress)
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
    let bnRpcAddress =  A.getRpcAddress (routingRuntime^.bnAddress)
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
    let bnRpcAddress  =  A.getRpcAddress (routingRuntime^.bnAddress)
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
            void $ L.notify (A.getUdpAddress reciverAddress) hello
        _ -> pure ()

--

udpRoutingHandlers routingRuntime = do
    L.handler $ acceptHello             routingRuntime
    L.handler $ acceptConnectResponse   routingRuntime
    L.handler $ acceptNextForYou        routingRuntime
    L.handler $ acceptHelloFromBn       routingRuntime

rpcRoutingHandlers routingRuntime = do
    L.method rpcPingPong
    L.method $ connectMapRequest routingRuntime

acceptHelloFromBn :: RoutingRuntime -> HelloToBnResponce -> D.Connection D.Udp -> L.NodeL ()
acceptHelloFromBn routingRuntime bnHello con = do
    L.close con
    when (verifyHelloToBnResponce bnHello) $
        L.writeVarIO (routingRuntime ^. hostAddress) $ Just (bnHello ^. hostAddress)

acceptHello :: RoutingRuntime -> RoutingHello -> D.Connection D.Udp -> L.NodeL ()
acceptHello routingRuntime routingHello con = do
    L.close con
    let senderAddress :: A.NodeAddress
        senderAddress  = routingHello ^. nodeAddress
    let senderId       = senderAddress ^. A.nodeId
    let myId           = routingRuntime ^. nodeId
    when (verifyRoutingHello routingHello) $ do
        connects <- getConnects routingRuntime
        let nextAddres = nextForHello myId senderId connects
        
        whenJust nextAddres $ \reciverAddress ->
            void $ L.notify (A.getUdpAddress reciverAddress) routingHello
        
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

acceptConnectResponse :: RoutingRuntime -> A.NodeAddress -> D.Connection D.Udp -> L.NodeL ()
acceptConnectResponse routingRuntime address con = do
    mAddress <- getMyNodeAddress routingRuntime
    whenJust mAddress $ \myAddress -> do 
        let nId = address ^. A.nodeId
        when (myAddress /= address) $
            L.modifyVarIO (routingRuntime ^. connectMap) (addToMap nId address)
    L.close con

udpBroadcastRecivedMessage
    :: (ToJSON msg, Typeable msg, Serialize msg)
    => RoutingRuntime -> (msg -> L.NodeL ()) -> msg ->  D.Connection D.Udp -> L.NodeL ()
udpBroadcastRecivedMessage routingRuntime handler message conn = do
    L.close conn
    whenM (sendUdpBroadcast routingRuntime message) $ handler message

--
connectMapRequest :: RoutingRuntime -> M.ConnectMapRequest -> L.NodeL [A.NodeAddress]
connectMapRequest nodeRuntime _ = 
    (snd <$>) . fromChordRouteMap <$> L.readVarIO (nodeRuntime ^. connectMap)

sendUdpBroadcast routingRuntime message = do
    needToProcessing <- L.atomically $ do
        let messageHash = D.toHashGeneric message
        familiarMessage <- isInFilter routingRuntime messageHash
        unless familiarMessage $ registerMsg routingRuntime messageHash
        pure $ not familiarMessage
    
    when needToProcessing $ do
        -- resending of message
        connectsToResending <- fromChordRouteMap <$> L.readVarIO (routingRuntime ^. connectMap)
        
        forM_ connectsToResending $ \nodeAddress ->
            void $ L.notify (A.getUdpAddress . snd $ nodeAddress) message
    pure needToProcessing 

udpForwardIfNeeded routingRuntime msg f = do
    let reciverId = msg^.nodeReciverId
    let myId      = routingRuntime^.nodeId
    let time      = msg^.timeToLive
    when (time > 0 && myId /= reciverId) $ do
        L.logInfo $ "Resending to " <> show reciverId
        connects <- L.readVarIO (routingRuntime ^. connectMap)
        let nextReciver = findNextResender reciverId connects
        whenJust nextReciver $ \(_, address) ->
            void $ L.notify (A.getUdpAddress address) (msg & timeToLive .~ time - 1)
        unless (isJust nextReciver) $ L.logError "Connection map is empty. Fail of resending."
    when (myId == reciverId) $ f msg
