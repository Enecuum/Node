module Enecuum.Assets.Services.Routing.RoutingWorker (runRouting) where

import qualified Data.Sequence                            as Seq
import qualified Data.Set                                 as Set
import qualified Enecuum.Assets.Nodes.Address             as A
import qualified Enecuum.Assets.Nodes.Messages            as M
import           Enecuum.Assets.Services.Routing.Messages
import           Enecuum.Assets.Services.Routing.RuntimeData
import qualified Enecuum.Domain                           as D
import qualified Enecuum.Framework.Lens                   as Lens
import qualified Enecuum.Language                         as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap

runRouting :: D.NodePorts -> D.NodeId -> BnAddress -> L.NodeDefinitionL RoutingRuntime
runRouting nodePorts' myNodeId' bnAdress' = do
    myNodeAddress'  <- registerWithBn nodePorts' myNodeId' bnAdress'
    routingData     <- L.scenario $ makeRoutingRuntimeData myNodeAddress' bnAdress'
    routingWorker routingData
    pure routingData

routingWorker :: RoutingRuntime -> L.NodeDefinitionL ()
routingWorker routingRuntime = do
    let bnUdpAddress = A.getUdpAddress (routingRuntime ^. bnAddress)

    builtIntoTheNetwork routingRuntime
    -- delete connects if now exist better
    L.periodic (1000 * 1000)  $ clearingOfConnects routingRuntime
    -- refine structure with clockwise direction
    L.periodic (1000 * 10000) $ successorsRequest  routingRuntime
    -- refine structure with anti-clockwise direction
    L.periodic (1000 * 1000)  $ sendHelloToPrevious routingRuntime
    -- cleare list of "familiar" messages
    L.periodic (1000 * 1000)  $
        L.modifyVarIO (routingRuntime ^. msgFilter)
            (\seq -> Set.empty Seq.<| Seq.deleteAt 2 seq)
    -- delete dead nodes
    L.periodic (1000 * 1000)  $ do
        deadNodes <- pingConnects =<< getConnects routingRuntime
        forM_ deadNodes $ \hash -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ removeFromMap hash
            L.notify bnUdpAddress $ M.IsDead hash

builtIntoTheNetwork :: RoutingRuntime -> L.NodeDefinitionL ()
builtIntoTheNetwork routingRuntime =
    forM_ [connecRequests (keySize - 1), nextRequest, sendHelloToPrevious]
        $ \action -> L.scenario $ action routingRuntime

-- TODO: Add real private key
registerWithBn :: D.NodePorts -> D.NodeId -> BnAddress -> L.NodeDefinitionL D.NodeAddress
registerWithBn nodePorts' myNodeId' bnAddress' = do
    let privateKey   =  True
    -- Ask boot node about our current node address (until we get the answer).
    helloToBn        <- makeHelloToBn privateKey nodePorts' myNodeId'
    let takeAddress i =  do
            void $ L.notify (A.getUdpAddress bnAddress') helloToBn
            eAddress <- L.makeRpcRequest (A.getRpcAddress bnAddress') $ AddressRequest myNodeId'
            case eAddress of
                Right address -> pure address
                Left err -> do
                    L.logError $ "Error " <> show i <> " in address accepting: " <> err
                    L.delay $ i * 1000 * 1000
                    takeAddress $ i + 1
    L.scenario $ takeAddress 0

sendHelloToPrevious :: RoutingRuntime -> L.NodeL ()
sendHelloToPrevious routingRuntime = do
    connects      <- getConnects      routingRuntime
    let mAddress  =  findNextForHash (getMyNodeId routingRuntime) connects
    whenJust mAddress $ \(_, receiverAddress) -> do
        let privateKey  = True
        hello <- makeRoutingHello privateKey (routingRuntime ^. myNodeAddres)
        void $ L.notify (A.getUdpAddress receiverAddress) hello

-- check if all connections are alive
-- return list of "dead connects"
pingConnects :: ChordRouteMap D.NodeAddress -> L.NodeL [D.NodeId]
pingConnects nodes = do
    deadNodes <- forM (elems nodes) $ \(nodeId, address) -> do
        res <- L.makeRpcRequest (A.getRpcAddress address) M.Ping
        pure $ case res of
            Right M.Pong -> Nothing
            Left  _      -> Just nodeId
    pure $ catMaybes deadNodes

-- sending requests to the network to clarify the map of connections
-- successors can be "better" connects than current connects.
successorsRequest :: RoutingRuntime -> L.NodeL ()
successorsRequest routingRuntime = do
    -- clarify the map of connections
    connects <- getConnects routingRuntime
    let myNodeId = getMyNodeId routingRuntime
    let loop i = do
            let mAddress = snd <$> findInMapNByKey (\h j -> D.hashToWord64 h + 2 ^ j) i myNodeId connects
            whenJust mAddress $ \address -> do
                (eAddress :: Either Text D.NodeAddress) <- L.makeRpcRequest (A.getRpcAddress address) $ M.ConnectRequest myNodeId i
                whenRight eAddress $ \(address :: D.NodeAddress) ->
                    unless (address == routingRuntime ^. myNodeAddres) $ do
                        L.modifyVarIO (routingRuntime ^. connectMap) (addToMap (address ^. Lens.nodeId) address)
                        when (i /= 0) $ loop (i - 1)
                whenLeft eAddress $ \_ -> when (i /= 0) $ loop (i - 1)
    loop (keySize - 1)

connecRequests :: Word64 -> RoutingRuntime -> L.NodeL ()
connecRequests i routingRuntime = when (i > 0) $ do
    let bnRpcAddress =  A.getRpcAddress (routingRuntime^.bnAddress)
    maybeAddress <- L.makeRpcRequest bnRpcAddress $ M.ConnectRequest (routingRuntime ^. myNodeAddres . Lens.nodeId) i
    case maybeAddress of
        Right (receivedNodeId, address) | routingRuntime ^. myNodeAddres /= address -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ addToMap receivedNodeId address
            connecRequests (i - 1) routingRuntime
        _ -> pure ()

nextRequest :: RoutingRuntime -> L.NodeL ()
nextRequest routingRuntime = do
    let bnRpcAddress  =  A.getRpcAddress (routingRuntime^.bnAddress)
    nextForMe        <- L.makeRpcRequest bnRpcAddress $ M.NextForMe (getMyNodeId routingRuntime)
    case nextForMe of
        Right (receivedNodeId, address) | routingRuntime ^. myNodeAddres /= address ->
            L.modifyVarIO (routingRuntime ^. connectMap) (addToMap receivedNodeId address)
        _ -> pure ()

-- clear list of connects ( connect must satisfy the prerequisites of the chord algorithm )
clearingOfConnects :: RoutingRuntime -> L.NodeL ()
clearingOfConnects routingRuntime = do
    connects <- getConnects routingRuntime
    L.atomically $ do
        let nextForMe   = maybeToList $ findNextForHash (getMyNodeId routingRuntime) connects
        let fingerNodes = findInMap (getMyNodeId routingRuntime) connects
        L.writeVar
            (routingRuntime ^. connectMap)
            (toChordRouteMap $ nextForMe <> fingerNodes)
