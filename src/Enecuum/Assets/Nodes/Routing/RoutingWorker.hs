module Enecuum.Assets.Nodes.Routing.RoutingWorker (runRouting) where

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
import           Enecuum.Assets.Nodes.Routing.RuntimeData

runRouting :: A.NodePorts -> A.NodeId -> BnAddress -> L.NodeDefinitionL RoutingRuntime
runRouting nodePorts' myNodeId' bnAdress' = do
    myNodeAddress'  <- registerWithBn nodePorts' myNodeId' bnAdress'
    routingData     <- L.scenario $ makeRoutingRuntimeData myNodeAddress' bnAdress'
    routingWorker routingData
    pure routingData

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

builtIntoTheNetwork :: RoutingRuntime -> L.NodeDefinitionL ()
builtIntoTheNetwork routingRuntime =
    forM_ [connecRequests 63, nextRequest, sendHelloToPrevius]
        $ \action -> L.scenario $ action routingRuntime

-- TODO: Add real private key
registerWithBn :: A.NodePorts -> A.NodeId -> BnAddress -> L.NodeDefinitionL A.NodeAddress
registerWithBn nodePorts' myNodeId' bnAddress' = do
    let privateKey   =  True
    -- Ask the BN for what address we are in until she answers.
    helloToBn        <- makeHelloToBn privateKey nodePorts' myNodeId'
    let takeAddress  =  do
            void $ L.notify (A.getUdpAddress bnAddress') helloToBn
            eAddress <- L.makeRpcRequest (A.getRpcAddress bnAddress') $ AddressRequest myNodeId' 
            case eAddress of
                Right address -> pure address
                Left err -> do
                    L.logError $ "Error in address accepting: " <> err
                    takeAddress
    L.scenario takeAddress

sendHelloToPrevius :: RoutingRuntime -> L.NodeL () 
sendHelloToPrevius routingRuntime = do
    connects      <- getConnects      routingRuntime
    let mAddress  =  findNextForHash (routingRuntime ^. myNodeAddres . A.nodeId) connects
    whenJust mAddress $ \(_, reciverAddress) -> do
        let privateKey  = True
        hello <- makeRoutingHello privateKey (routingRuntime ^. myNodeAddres)
        void $ L.notify (A.getUdpAddress reciverAddress) hello

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

-- sending requests to the network to clarify the map of connections
-- successors may be better connections than those that are now.
successorsRequest :: RoutingRuntime -> L.NodeL ()
successorsRequest routingRuntime = do
    -- clarify the map of connections
    connects <- getConnects routingRuntime
    let myNodeId = routingRuntime ^. myNodeAddres . A.nodeId
    let loop i = do
            let mAddress = snd <$> findInMapNByKey (\h j -> D.hashToWord64 h + 2 ^ j) i myNodeId connects
            whenJust mAddress $ \address -> do
                (eAddress :: Either Text A.NodeAddress) <- L.makeRpcRequest (A.getRpcAddress address) $ M.ConnectRequest myNodeId i
                whenRight eAddress $ \(address :: A.NodeAddress) ->
                    unless (address == routingRuntime ^. myNodeAddres) $ do
                        L.modifyVarIO (routingRuntime ^. connectMap) (addToMap (address ^. A.nodeId) address)
                        when (i /= 0) $ loop (i - 1)
                whenLeft eAddress $ \_ -> when (i /= 0) $ loop (i - 1)
    loop 63

connecRequests :: Word64 -> RoutingRuntime -> L.NodeL ()
connecRequests i routingRuntime = when (i > 0) $ do
    let bnRpcAddress =  A.getRpcAddress (routingRuntime^.bnAddress)
    maybeAddress <- L.makeRpcRequest bnRpcAddress $ M.ConnectRequest (routingRuntime ^. myNodeAddres . A.nodeId) i
    case maybeAddress of
        Right (recivedNodeId, address) | routingRuntime ^. myNodeAddres /= address -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ addToMap recivedNodeId address
            connecRequests (i - 1) routingRuntime
        _ -> pure ()

nextRequest :: RoutingRuntime -> L.NodeL ()
nextRequest routingRuntime = do
    let bnRpcAddress  =  A.getRpcAddress (routingRuntime^.bnAddress)
    nextForMe        <- L.makeRpcRequest bnRpcAddress $ M.NextForMe (routingRuntime ^. myNodeAddres . A.nodeId)
    case nextForMe of
        Right (recivedNodeId, address) | routingRuntime ^. myNodeAddres /= address ->
            L.modifyVarIO (routingRuntime ^. connectMap) (addToMap recivedNodeId address)
        _ -> pure ()

-- leave in the list of connections only those
-- that satisfy the conditions of the chord algorithm
clearingOfConnects :: RoutingRuntime -> L.NodeL ()
clearingOfConnects routingRuntime = do
    connects <- getConnects routingRuntime
    L.atomically $ do
        let nextForMe   = maybeToList $ findNextForHash (routingRuntime ^. myNodeAddres . A.nodeId) connects
        let fingerNodes = findInMap (routingRuntime ^. myNodeAddres . A.nodeId) connects
        L.writeVar
            (routingRuntime ^. connectMap)
            (toChordRouteMap $ nextForMe <> fingerNodes)
