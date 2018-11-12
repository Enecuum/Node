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
    , _connectMap  :: D.StateVar (ChordRouteMap D.Address)
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

--

clearingOfConnects :: RoutingRuntime -> L.NodeL ()
clearingOfConnects myHash nodeData = L.atomically $ do
    nodes <- L.readVar (nodeData ^. netNodes)
    let filteredNodes   = maybeToList (findNextForHash myHash nodes) <> findInMap myHash nodes
    let filteredNodeMap = toChordRouteMap filteredNodes
    L.writeVar (nodeData ^. netNodes) filteredNodeMap

successorsRequest :: RoutingRuntime -> L.NodeL ()
successorsRequest myAddress nodeData = do
    nodes <- L.readVarIO (nodeData ^. netNodes)
    forM_ nodes $ \(_, addr) -> void $
        L.notify addr $ M.NextForYou myAddress

routingWorker :: RoutingRuntime -> L.NodeDefinitionL ()
routingWorker routingRuntime = do
    builtIntoTheNetwork routingRuntime

    periodic (1000 * 1000)  $ clearingOfConnects routingRuntime
    periodic (1000 * 10000) $ successorsRequest  routingRuntime
    periodic (1000 * 1000)  $ sendHelloToPrevius routingRuntime
    periodic (1000 * 1000)  $ do
        deadNodes <- pingConnects =<< L.readVarIO (nodeData ^. netNodes)
        forM_ deadNodes $ \hash -> do
            L.atomically $ L.modifyVar (nodeData ^. netNodes) $ removeFromMap hash
            let D.Address bnHost bnPort = A.bnAddress
            L.notify (D.Address bnHost (bnPort - 1000)) $ M.IsDead hash
--
builtIntoTheNetwork :: RoutingRuntime -> L.NodeDefinitionL ()
builtIntoTheNetwork routingRuntime = L.scenario $ 
    forM_ [registerWithBn, connecRequests 63, nextRequest, sendHelloToPrevius]
        $ \action -> action routingRuntime

registerWithBn :: RoutingRuntime -> L.NodeL ()
registerWithBn routingRuntime = do
    let bnUdpAddress = getUdpAddress (routingRuntime^.bnAddress)
    let privateKey   = True
    helloToBn <- makeHelloToBn privateKey (routingRuntime^.nodePorts) (routingRuntime^.nodeId)
    let takeAddress  = do
            L.notify bnUdpAddress helloToBn
            L.delay 1000000
            unlesM (isJust <$> readVarIO (routingRuntime ^. hostAddress)) takeAddress
    L.process takeAddress
    void $ L.await (routingRuntime^.hostAddress)

connecRequests :: Word64 -> RoutingRuntime -> L.NodeL ()
connecRequests i routingRuntime = when (i > 0) $ do
    let bnRpcAddress = getRpcAddress (routingRuntime^.bnAddress)
    let myNodeId     = routingRuntime ^. nodeId
    maybeAddress <- L.makeRpcRequest bnRpcAddress $ M.ConnectRequest myNodeId i
    myAddress    <- await (routingRuntime^.hostAddress)
    case maybeAddress of
        Right (recivedNodeId, address) | myAddress /= address -> do
            L.modifyVarIO (routingRuntime ^. connectMap) $ addToMap recivedNodeId address
            connecRequests (i - 1) routingRuntime
        _ -> pure ()

nextRequest :: RoutingRuntime -> L.NodeL ()
nextRequest routingRuntime = do
    let bnRpcAddress = getRpcAddress (routingRuntime^.bnAddress)
    let myNodeId     = routingRuntime ^. nodeId
    myAddress <- await (routingRuntime^.hostAddress)
    nextForMe <- L.makeRpcRequest bnRpcAddress $ M.NextForMe myNodeId
    case nextForMe of
        Right (recivedNodeId, address) | myAddress /= address ->
            L.modifyVarIO (nodeData ^. connectMap) (addToMap recivedNodeId address)
        _ -> pure ()

sendHelloToPrevius :: RoutingRuntime -> L.NodeL () 
sendHelloToPrevius routingRuntime = do
    connectMap <- L.readVarIO (routingRuntime ^. connectMap)
    let myNodeId    = routingRuntime ^. nodeId
    myNodeAddress <- getMyNodeAddress routingRuntime
    let mAddress    = findNextForHash myNodeId connectMap
    case (myNodeAddress, mAddress) of
        (Just myAddress, Just reciverAddress) -> do  
            let privateKey  = True
            hello <- makeRoutingHello privateKey myNodeAddress
            void $ L.notify address hello
        _ -> pure ()