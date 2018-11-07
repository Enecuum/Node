{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.Routing.NN (nnNode, NN, NodeConfig (..)) where

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

type MyNodeHash      = D.StringHash
type SenderNodeHash  = D.StringHash

data NNNodeData = NNNodeData
    { _status          :: D.StateVar L.NodeStatus
    , _netNodes        :: D.StateVar (ChordRouteMap D.Address)
    , _nodePort        :: D.StateVar (Maybe D.PortNumber)
    , _routingMessages :: D.StateVar [Text]
    }
makeFieldsNoPrefix ''NNNodeData

data NN = NN
    deriving (Show, Generic)

data instance NodeConfig NN = NNConfig
    { _dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node NN where
    data NodeScenario NN = NNS
        deriving (Show, Generic)
    getNodeScript NNS = nnNode' Nothing


instance ToJSON   NN                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON NN                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig NN)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig NN)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario NN) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario NN) where parseJSON = J.genericParseJSON nodeConfigJsonOptions


newtype Start = Start D.PortNumber deriving (Read)

startNode :: NNNodeData -> Start -> L.NodeL Text
startNode nodeData (Start port) = L.atomically $ do
    currentPort <- L.readVar (nodeData^.nodePort)
    unless (isJust currentPort) $
        L.writeVar (nodeData^.nodePort) $ Just port
    pure $ if isJust currentPort
        then "Node is already running."
        else "The port is accepted, the node is started."

initNN :: Maybe D.PortNumber -> L.NodeDefinitionL NNNodeData
initNN maybePort = L.atomically
    (NNNodeData <$> L.newVar L.NodeActing <*> L.newVar mempty <*> L.newVar maybePort <*> L.newVar [])

awaitPort :: NNNodeData -> L.NodeDefinitionL D.PortNumber
awaitPort nodeData = L.atomically $ do
    currentPort <- L.readVar $ nodeData ^. nodePort
    case currentPort of
        Just port -> pure port
        Nothing   -> L.retry

connectToBN :: D.MyAddress -> D.Address -> NNNodeData -> L.NodeDefinitionL ()
connectToBN myAddress bnAddress nodeData = do
    let myHash = D.toHashGeneric myAddress
    -- TODO add proccesing of error
    _ :: Either Text M.SuccessMsg <- L.scenario $ L.makeRpcRequest bnAddress $ M.Hello myHash myAddress
    L.scenario $ do
        connecRequests 63 nodeData bnAddress myHash myAddress
        nextRequest nodeData bnAddress myHash myAddress
        sendHelloToPrevius nodeData myHash myAddress

connecRequests :: Word64 -> NNNodeData -> D.Address -> MyNodeHash -> D.MyAddress -> L.NodeL ()
connecRequests i nodeData bnAddress hash myAddress = when (i > 0) $ do
    maybeAddress :: Either Text (D.StringHash, D.Address) <- L.makeRpcRequest bnAddress $ M.ConnectRequest hash i
    case maybeAddress of
        Right (recivedHash, address) | myAddress /= address -> do
            L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap recivedHash address)
            connecRequests (i - 1) nodeData bnAddress hash myAddress
        _ -> pure ()

nextRequest :: NNNodeData -> D.Address -> D.StringHash -> D.Address -> L.NodeL ()
nextRequest nodeData bnAddress myHash myAddress = do
    nextForMe :: Either Text (D.StringHash, D.Address) <-
        L.makeRpcRequest bnAddress $ M.NextForMe myHash
    case nextForMe of
        Right (recivedHash, address) | myAddress /= address ->
            L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap recivedHash address)
        _ -> pure ()

sendHelloToPrevius :: NNNodeData -> D.StringHash -> D.Address -> L.NodeL () 
sendHelloToPrevius nodeData myHash myAddress = do
    connectMap <- L.readVarIO (nodeData ^. netNodes)
    let mAddress = findNextForHash myHash connectMap
    whenJust mAddress $ \(_, address) ->
        void $ L.notify address $ M.Hello myHash myAddress

acceptHello :: NNNodeData -> MyNodeHash -> M.Hello -> D.Connection D.Udp -> L.NodeL ()
acceptHello nodeData myHash (M.Hello senderHash senderAddress) con = do
    L.close con
    connectMap <- L.readVarIO (nodeData ^. netNodes)
    let nextAddres = nextForHello myHash senderHash connectMap
    
    whenJust nextAddres $ \reciverAddress ->
        void $ L.notify reciverAddress $ M.Hello senderHash senderAddress
    
    L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap senderHash senderAddress)
    

acceptConnectResponse :: NNNodeData -> D.Address -> M.ConnectResponse -> D.Connection D.Udp -> L.NodeL ()
acceptConnectResponse nodeData myAddress (M.ConnectResponse hash address) con = do
    when (myAddress /= address) $
        L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap hash address)
    L.close con

acceptNextForYou :: NNNodeData -> D.StringHash -> M.NextForYou -> D.Connection D.Udp -> L.NodeL ()
acceptNextForYou nodeData hash (M.NextForYou senderAddress) conn = do
    L.close conn
    maybeAddress <- L.atomically $ do
        connectMap <- L.readVar (nodeData ^. netNodes)
        pure $ findNextForHash hash connectMap
    whenJust maybeAddress $ \(h, address) ->
        void $ L.notify senderAddress $ M.ConnectResponse h address

clearingOfConnects :: D.StringHash -> NNNodeData -> L.NodeL ()
clearingOfConnects myHash nodeData = L.atomically $ do
    nodes <- L.readVar (nodeData ^. netNodes)
    let filteredNodes   = maybeToList (findNextForHash myHash nodes) <> findInMap myHash nodes
    let filteredNodeMap = toChordRouteMap filteredNodes
    L.writeVar (nodeData ^. netNodes) filteredNodeMap

successorsRequest :: D.Address -> NNNodeData -> L.NodeL ()
successorsRequest myAddress nodeData = do
    nodes <- L.readVarIO (nodeData ^. netNodes)
    forM_ nodes $ \(_, addr) -> void $
        L.notify addr $ M.NextForYou myAddress

testPorts :: [D.PortNumber]
testPorts = [5001..5010]

nodesMap :: Map D.StringHash D.Address
nodesMap = Map.fromList $ map (\node -> (D.toHashGeneric node, node)) nnNodes
    where nnNodes = map (D.Address A.localhost) testPorts

acceptSendTo
    :: NNNodeData -> D.StringHash -> M.SendMsgTo -> D.Connection D.Udp -> L.NodeL ()
acceptSendTo nodeData myHash (M.SendMsgTo hash i msg) conn = do
    L.close conn
    let hashInfo = case Map.lookup hash nodesMap of
            Nothing -> show hash
            Just address -> show address
    let mes = "Received msg: \"" <>  msg <> "\" for " <> hashInfo <> " time to live " <> show i
    L.logInfo mes

    when (myHash == hash) $ do
        L.atomically $ L.modifyVar (nodeData ^. routingMessages) (mes :)
        L.logInfo "I'm receiver."

    when (i >= 0 && myHash /= hash) $ do
        rm <- L.readVarIO (nodeData ^. netNodes)
        whenJust (findNextResender hash rm) $ \(h, address) -> do
            L.logInfo $ "Resending to: " <> show h
            void $ L.notify address (M.SendMsgTo hash (i-1) msg)

connectMapRequest :: NNNodeData -> M.ConnectMapRequest -> L.NodeL [(D.StringHash, D.Address)]
connectMapRequest nodeData _ = do
    nodes <- L.readVarIO (nodeData ^. netNodes)
    pure $ fromChordRouteMap nodes

getRoutingMessages :: NNNodeData -> M.GetRoutingMessages -> L.NodeL [Text]
getRoutingMessages nodeData _ = L.readVarIO (nodeData ^. routingMessages)

nnNode :: Maybe D.PortNumber -> L.NodeDefinitionL ()
nnNode port = nnNode' port (NNConfig 42)

nnNode' :: Maybe D.PortNumber -> NodeConfig NN -> L.NodeDefinitionL ()
nnNode' maybePort _ = do
    L.nodeTag "NN node"
    L.logInfo "Starting of NN node"
    nodeData    <- initNN maybePort
    L.std $ do
    -- network
        L.stdHandler $ startNode nodeData
        L.stdHandler $ L.stopNodeHandler nodeData

    -- routing
    port        <- awaitPort nodeData
    let myAddress = D.Address "127.0.0.1" port
    let myHash    = D.toHashGeneric myAddress
    L.logInfo $ show myHash
    connectToBN myAddress A.bnAddress nodeData

    -- make rpc port different from udp via (port - 1000)
    L.serving D.Rpc (port - 1000) $ do
        L.method  $  connectMapRequest    nodeData
        L.method  $  getRoutingMessages   nodeData
        L.method     rpcPingPong

    L.serving D.Udp port $ do
        L.handler $ acceptHello           nodeData myHash
        L.handler $ acceptConnectResponse nodeData myAddress
        L.handler $ acceptNextForYou      nodeData myHash
        L.handler $ acceptSendTo          nodeData myHash

    L.process $ forever $ do
        L.delay $ 1000 * 1000
        clearingOfConnects myHash nodeData

    L.process $ forever $ do
        L.delay $ 1000 * 10000
        successorsRequest myAddress nodeData

    L.process $ forever $ do
        L.delay $ 1000 * 1000
        deadNodes <- pingConnects =<< L.readVarIO (nodeData ^. netNodes)
        forM_ deadNodes $ \hash -> do
            L.atomically $ L.modifyVar (nodeData ^. netNodes) $ removeFromMap hash
            let D.Address bnHost bnPort = A.bnAddress
            L.notify (D.Address bnHost (bnPort - 1000)) $ M.IsDead hash


    L.process $ forever $ do
        L.delay $ 1000 * 1000
        sendHelloToPrevius nodeData myHash myAddress

    L.awaitNodeFinished nodeData
