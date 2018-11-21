module Enecuum.Assets.Nodes.Routing.RoutingWorker where

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
            eAddress <- L.makeRpcRequest (A.getRpcAddress (routingRuntime^.bnAddress)) $ AddressRequest (routingRuntime^.myNodeId)
            whenRight eAddress $ \(address :: A.NodeAddress) -> 
                L.writeVarIO (routingRuntime ^. hostAddress) $ Just (address ^. A.nodeHost)
            whenLeft eAddress $ \err -> L.logError $ "Error in address accepting: " <> err
    L.scenario takeAddress

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
successorsRequest routingRuntime =
    -- clarify the map of connections
    -- if we have a return address that we can answer
    whenJustM (getMyNodeAddress routingRuntime) $ \myNodeAddress -> do
        conects <- getConnects routingRuntime
        forM_ conects $ \(_, addr) ->
            -- ask who for the respondent next
            void $ L.notify (A.getUdpAddress addr) $ NextForYou $ A.getUdpAddress myNodeAddress

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
