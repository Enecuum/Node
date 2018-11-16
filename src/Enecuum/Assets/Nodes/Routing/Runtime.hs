{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.Routing.Runtime where

import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Domain                   as D
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
    , _myNodeId    :: A.NodeId
    , _bnAddress   :: A.NodeAddress
    , _connectMap  :: D.StateVar (ChordRouteMap A.NodeAddress)
    , _msgFilter   :: D.StateVar (Seq.Seq (Set.Set D.StringHash))
    }
makeFieldsNoPrefix ''RoutingRuntime

makeRoutingRuntimeData :: A.NodePorts -> A.NodeId -> BnAddress -> L.NodeL RoutingRuntime
makeRoutingRuntimeData nodePorts' nodeId' bnAdress' = do
    myHostAddress <- L.newVarIO Nothing
    myConnectMap  <- L.newVarIO mempty
    msgFilterSets <- L.newVarIO $ Seq.fromList [Set.empty, Set.empty, Set.empty]
    pure $ RoutingRuntime myHostAddress nodePorts' nodeId' bnAdress' myConnectMap msgFilterSets

routingWorker :: RoutingRuntime -> L.NodeDefinitionL ()
routingWorker routingRuntime = do
    let bnUdpAddress = A.getUdpAddress (routingRuntime^.bnAddress)

    builtIntoTheNetwork routingRuntime
    -- delete connects if now exist better
    L.periodic (1000 * 1000)  $ clearingOfConnects routingRuntime
    -- clockwise structure refinement
    L.periodic (1000 * 10000) $ successorsRequest  routingRuntime
    -- anti-clockwise structure refinement
    L.periodic (1000 * 1000)  $ sendHelloToPrevius routingRuntime
    -- clearing of list for familiar messages
    L.periodic (1000 * 1000)  $
        L.modifyVarIO (routingRuntime ^. msgFilter)
            (\seq -> Set.empty Seq.<| Seq.deleteAt 2 seq)
    -- deleting of deads nodes
    L.periodic (1000 * 1000)  $ do
        deadNodes <- pingConnects =<< getConnects routingRuntime
        forM_ deadNodes $ \hash -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ removeFromMap hash
            L.notify bnUdpAddress $ M.IsDead hash

udpRoutingHandlers :: RoutingRuntime -> L.NetworkHandlerL D.Udp L.NodeL ()
udpRoutingHandlers routingRuntime = do
    L.handler $ acceptHello             routingRuntime
    L.handler $ acceptConnectResponse   routingRuntime
    L.handler $ acceptNextForYou        routingRuntime
    L.handler $ acceptHelloFromBn       routingRuntime

rpcRoutingHandlers :: RoutingRuntime -> L.RpcHandlerL L.NodeL ()
rpcRoutingHandlers routingRuntime = do
    L.method rpcPingPong
    L.method $ connectMapRequest routingRuntime

-- | Send udp broadcast if the message is new.
sendUdpBroadcast
    :: Typeable message
    => ToJSON message
    => Serialize message
    => RoutingRuntime -> message -> L.NodeL Bool
sendUdpBroadcast routingRuntime message = do
    -- check if the message is familiar
    -- if not, add to the list of familiar messages
    needToBroadcast <- L.atomically $ do
        let messageHash = D.toHashGeneric message
        familiarMessage <- isInFilter routingRuntime messageHash
        unless familiarMessage $ registerMsg routingRuntime messageHash
        pure $ not familiarMessage

    -- resending of message to everyone we know
    when needToBroadcast $ do
        connectsToResending <- fromChordRouteMap <$> L.readVarIO (routingRuntime ^. connectMap)
        forM_ connectsToResending $ \receiverAddress ->
            void $ L.notify (A.getUdpAddress . snd $ receiverAddress) message
    pure needToBroadcast

-- | Forward and after proccesing udp message if it needed
udpForwardIfNeeded
    :: HasTimeToLive message Int
    => ToJSON message
    => Typeable message
    => HasNodeReciverId message D.StringHash
    => RoutingRuntime -> message -> (message -> L.NodeL ()) -> L.NodeL ()
udpForwardIfNeeded routingRuntime message handler = do
    let reciverId = message ^. nodeReciverId
    -- process message if I am a recipient
    if routingRuntime^.myNodeId == reciverId
        then handler message

        -- forward the message further if it is not yet old.
        else when (message ^. timeToLive > 0) $ do
            L.logInfo $ "Resending to " <> show reciverId
            connects <- L.readVarIO (routingRuntime ^. connectMap)
            let nextReciver = findNextResender reciverId connects
            whenJust nextReciver $ \(_, address) ->
                void $ L.notify (A.getUdpAddress address) (message & timeToLive %~ (\x -> x - 1))
            
            unless (isJust nextReciver) $ L.logError "Connection map is empty. Fail of resending."

-- forward and proccessing the received message if necessary
udpBroadcastRecivedMessage
    :: (ToJSON msg, Typeable msg, Serialize msg)
    => RoutingRuntime -> (msg -> L.NodeL ()) -> msg ->  D.Connection D.Udp -> L.NodeL ()
udpBroadcastRecivedMessage routingRuntime handler message conn = do
    L.close conn
    -- it is possible to forward only new messages
    -- if the message did not broadcasted, then we have already processed it
    whenM (sendUdpBroadcast routingRuntime message) $ handler message

--------------------------------------------------------------------------------
--                          INTERNAL
--------------------------------------------------------------------------------
-- | Check if the message is familiar?
isInFilter :: RoutingRuntime -> D.StringHash -> L.StateL Bool
isInFilter routingRuntime messageHash = do
    sets <- L.readVar $ routingRuntime ^. msgFilter
    pure $ any (\i -> messageHash `Set.member` Seq.index sets i) [0..2]

-- | Add the message to list of familiar message.
registerMsg :: RoutingRuntime -> D.StringHash -> L.StateL ()
registerMsg routingRuntime messageHash =
    L.modifyVar (routingRuntime ^. msgFilter) (Seq.adjust (Set.insert messageHash) 0)

itIsMyAddress :: RoutingRuntime -> A.NodeAddress -> L.NodeL Bool
itIsMyAddress routingRuntime address = do
    myAddress <- getMyNodeAddress routingRuntime
    pure $ Just address == myAddress

getMyNodeAddress :: RoutingRuntime -> L.NodeL (Maybe A.NodeAddress)
getMyNodeAddress routingRuntime = do
    mHost <- L.readVarIO (routingRuntime ^. hostAddress)
    case mHost of
        Just host -> pure $ Just $
            A.NodeAddress host (routingRuntime ^. nodePorts) (routingRuntime ^. myNodeId)
        Nothing   -> pure Nothing

getConnects :: RoutingRuntime -> L.NodeL (ChordRouteMap A.NodeAddress)
getConnects routingRuntime = L.readVarIO (routingRuntime ^. connectMap)

-- leave in the list of connections only those
-- that satisfy the conditions of the chord algorithm
clearingOfConnects :: RoutingRuntime -> L.NodeL ()
clearingOfConnects routingRuntime = do
    connects <- getConnects routingRuntime
    L.atomically $ do
        let nextForMe   = maybeToList $ findNextForHash (routingRuntime ^. myNodeId) connects
        let fingerNodes = findInMap (routingRuntime ^. myNodeId) connects
        L.writeVar
            (routingRuntime ^. connectMap)
            (toChordRouteMap $ nextForMe <> fingerNodes)

-- sending requests to the network to clarify the map of connections
-- successors may be better connections than those that are now.
successorsRequest :: RoutingRuntime -> L.NodeL ()
successorsRequest routingRuntime =
    -- clarify the map of connections
    -- if we have a return address that we can answer
    whenJustM (getMyNodeAddress routingRuntime) $ \myNodeAddress -> do
        conects <- getConnects routingRuntime
        forM_ conects $ \(_, addr) ->
            -- ask who for the respondent next
            void $ L.notify (A.getUdpAddress addr) $ NextForYou $ A.getUdpAddress myNodeAddress

-- answer to the questioner who is successor for me
acceptNextForYou :: RoutingRuntime -> NextForYou -> D.Connection D.Udp -> L.NodeL ()
acceptNextForYou routingRuntime (NextForYou senderAddress) conn = do
    L.close conn
    connects <- getConnects routingRuntime
    let mAddress = snd <$> findNextForHash (routingRuntime ^. myNodeId) connects
    whenJust mAddress $ \address -> void $ L.notify senderAddress address

-- check if all connections are alive
-- return list of "dead connects"
pingConnects :: ChordRouteMap A.NodeAddress -> L.NodeL [A.NodeId]
pingConnects nodes = do
    deadNodes <- forM (elems nodes) $ \(nodeId, address) -> do
        res <- L.makeRpcRequest (A.getRpcAddress address) M.Ping
        pure $ case res of
            Right M.Pong -> Nothing
            Left  _      -> Just nodeId
    pure $ catMaybes deadNodes


builtIntoTheNetwork :: RoutingRuntime -> L.NodeDefinitionL ()
builtIntoTheNetwork routingRuntime = do
    registerWithBn routingRuntime
    forM_ [connecRequests 63, nextRequest, sendHelloToPrevius]
        $ \action -> L.scenario $ action routingRuntime

-- TODO: Add real private key
registerWithBn :: RoutingRuntime -> L.NodeDefinitionL ()
registerWithBn routingRuntime = do
    let privateKey   =  True
    -- Ask the BN for what address we are in until she answers.
    helloToBn        <- makeHelloToBn privateKey (routingRuntime^.nodePorts) (routingRuntime^.myNodeId)
    let takeAddress  =  do
            void $ L.notify (A.getUdpAddress (routingRuntime^.bnAddress)) helloToBn
            L.delay 1000000
            unlessM (isJust <$> L.readVarIO (routingRuntime ^. hostAddress)) takeAddress
    L.process takeAddress
    void $ L.await (routingRuntime^.hostAddress)

connecRequests :: Word64 -> RoutingRuntime -> L.NodeL ()
connecRequests i routingRuntime = when (i > 0) $ do
    let bnRpcAddress =  A.getRpcAddress (routingRuntime^.bnAddress)
    maybeAddress <- L.makeRpcRequest bnRpcAddress $ M.ConnectRequest (routingRuntime ^. myNodeId) i
    myAddress    <- getMyNodeAddress routingRuntime
    case maybeAddress of
        Right (recivedNodeId, address) | myAddress /= Just address -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ addToMap recivedNodeId address
            connecRequests (i - 1) routingRuntime
        _ -> pure ()

nextRequest :: RoutingRuntime -> L.NodeL ()
nextRequest routingRuntime = do
    let bnRpcAddress  =  A.getRpcAddress (routingRuntime^.bnAddress)
    myAddress        <- getMyNodeAddress routingRuntime
    nextForMe        <- L.makeRpcRequest bnRpcAddress $ M.NextForMe (routingRuntime ^. myNodeId)
    case nextForMe of
        Right (recivedNodeId, address) | myAddress /= Just address ->
            L.modifyVarIO (routingRuntime ^. connectMap) (addToMap recivedNodeId address)
        _ -> pure ()

sendHelloToPrevius :: RoutingRuntime -> L.NodeL () 
sendHelloToPrevius routingRuntime = do
    connects      <- getConnects      routingRuntime
    myNodeAddress <- getMyNodeAddress routingRuntime
    let mAddress  =  findNextForHash (routingRuntime ^. myNodeId) connects
    case (myNodeAddress, mAddress) of
        (Just myAddress, Just (_, reciverAddress)) -> do  
            let privateKey  = True
            hello <- makeRoutingHello privateKey myAddress
            void $ L.notify (A.getUdpAddress reciverAddress) hello
        _ -> pure ()

acceptHelloFromBn :: RoutingRuntime -> HelloToBnResponce -> D.Connection D.Udp -> L.NodeL ()
acceptHelloFromBn routingRuntime bnHello con = do
    L.close con
    when (verifyHelloToBnResponce bnHello) $
        L.writeVarIO (routingRuntime ^. hostAddress) $ Just (bnHello ^. hostAddress)

-- | Processing of messages forwarded to maintain the integrity of the network structure
--   clarifying the predecessor and successor relationship
acceptHello :: RoutingRuntime -> RoutingHello -> D.Connection D.Udp -> L.NodeL ()
acceptHello routingRuntime routingHello con = do
    L.close con
    when (verifyRoutingHello routingHello) $ do
        connects <- getConnects routingRuntime
        let senderAddress = routingHello ^. nodeAddress
        let senderNodeId  = senderAddress ^. A.nodeId
        let nextAddres    = nextForHello (routingRuntime ^. myNodeId) senderNodeId connects
        whenJust nextAddres $ \reciverAddress ->
            void $ L.notify (A.getUdpAddress reciverAddress) routingHello
        
        L.modifyVarIO
            (routingRuntime ^. connectMap)
            (addToMap senderNodeId senderAddress)

acceptConnectResponse :: RoutingRuntime -> A.NodeAddress -> D.Connection D.Udp -> L.NodeL ()
acceptConnectResponse routingRuntime address con = do
    L.close con
    -- if this address is not mine, then add it
    unlessM (itIsMyAddress routingRuntime address) $
        L.modifyVarIO (routingRuntime ^. connectMap) (addToMap (address ^. A.nodeId) address)

connectMapRequest :: RoutingRuntime -> M.ConnectMapRequest -> L.NodeL [A.NodeAddress]
connectMapRequest nodeRuntime _ =
    -- return all known connections
    (snd <$>) . fromChordRouteMap <$> L.readVarIO (nodeRuntime ^. connectMap)
