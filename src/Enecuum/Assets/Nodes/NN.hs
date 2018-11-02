{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Nodes.NN (nnNode, NN, NodeConfig (..)) where

import           Enecuum.Prelude
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Language               as L
import qualified Enecuum.Assets.Nodes.Address   as A
import qualified Enecuum.Assets.Nodes.Messages  as M
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Framework.Language.Extra (HasStatus)
import           Enecuum.Config
import qualified Data.Aeson                       as J

data NNNodeData = NNNodeData
    { _status   :: D.StateVar L.NodeStatus
    , _netNodes :: D.StateVar (ChordRouteMap D.Address)
    , _nodePort :: D.StateVar (Maybe D.PortNumber)
    }
makeFieldsNoPrefix ''NNNodeData

--
data NN = NN
    deriving (Show, Generic)

data instance NodeConfig NN = NNConfig
    { dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node NN where
    data NodeScenario NN = NNS
        deriving (Show, Generic)
    getNodeScript NNS = nnNode Nothing


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
    (NNNodeData <$> L.newVar L.NodeActing <*> L.newVar mempty <*> L.newVar maybePort)

awaitPort :: NNNodeData -> L.NodeDefinitionL D.PortNumber
awaitPort nodeData = L.atomically $ do
    currentPort <- L.readVar $ nodeData ^. nodePort
    case currentPort of
        Just port -> pure port
        Nothing   -> L.retry

connectToBN :: D.Address -> D.Address -> NNNodeData -> L.NodeDefinitionL ()
connectToBN myAddress bnAddress nodeData = do
    let hash = D.toHashGeneric myAddress
    -- TODO add proccesing of error
    _ :: Either Text M.SuccessMsg <- L.scenario $ L.makeRpcRequest bnAddress $ M.Hello hash myAddress
    let loop i = when (i > 0) $ do
            maybeAddress :: Either Text (D.StringHash, D.Address) <- L.scenario $ L.makeRpcRequest bnAddress $ M.ConnectRequest hash i
            case maybeAddress of
                Right (recivedHash, address) | myAddress /= address -> do
                    L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap recivedHash address)
                    loop (i - 1)
                _ -> pure ()
    loop 63

    nextForMe :: Either Text (D.StringHash, D.Address) <- L.scenario $
        L.makeRpcRequest bnAddress $ M.NextForMe hash
    case nextForMe of
        Right (recivedHash, address) | myAddress /= address ->
            L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap recivedHash address)
        _ -> pure ()

    previousForMe :: Either Text (D.StringHash, D.Address) <- L.scenario $
        L.makeRpcRequest bnAddress $ M.PreviousForMe hash
    case previousForMe of
        Right (_, address) | myAddress /= address ->
            void $ L.notify address $ M.Hello hash myAddress
        _ -> pure ()

acceptHello :: NNNodeData -> M.Hello -> D.Connection D.Udp -> L.NodeL ()
acceptHello nodeData (M.Hello hash address) con = do
    L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap hash address)
    L.close con

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

acceptSendTo
    :: NNNodeData -> D.StringHash -> M.SendTo -> D.Connection D.Udp -> L.NodeL ()
acceptSendTo nodeData myHash (M.SendTo hash i msg) conn = do
    L.close conn
    L.logInfo $ "Received msg: \"" <>  msg <> "\" for " <> show hash <> " time to live " <> show i 
    
    when (myHash == hash) $ L.logInfo "I'm receiver."
    
    when (i >= 0 && myHash /= hash) $ do
        rm <- L.readVarIO (nodeData ^. netNodes)
        whenJust (findNextResender hash rm) $ \(h, address) -> do
            L.logInfo $ "Resending to: " <> show h
            void $ L.notify address (M.SendTo hash (i-1) msg)

--
connectMapRequest :: NNNodeData -> M.ConnectMapRequest -> L.NodeL [(D.StringHash, D.Address)]
connectMapRequest nodeData _ = do
    nodes <- L.readVarIO (nodeData ^. netNodes)
    pure $ fromChordRouteMap nodes

nnNode :: Maybe D.PortNumber -> NodeConfig NN -> L.NodeDefinitionL ()
nnNode maybePort _ = do
    L.nodeTag "NN node"
    L.logInfo "Starting of NN node"
    nodeData    <- initNN maybePort
    L.std $ do
        L.stdHandler $ startNode nodeData
        L.stdHandler $ L.stopNodeHandler nodeData
    
    -- routing
    port        <- awaitPort nodeData
    let myAddress = D.Address "127.0.0.1" port
    let myHash    = D.toHashGeneric myAddress
    L.logInfo $ show myHash
    connectToBN myAddress A.bnAddress nodeData

    L.serving D.Rpc (port - 1000) $
        L.method  $  connectMapRequest     nodeData

    L.serving D.Udp port $ do
        L.handler $ acceptHello           nodeData
        L.handler $ acceptConnectResponse nodeData myAddress
        L.handler $ acceptNextForYou      nodeData myHash
        L.handler $ acceptSendTo          nodeData myHash

    L.process $ forever $ do
        L.delay $ 1000 * 1000
        clearingOfConnects myHash nodeData

    L.process $ forever $ do
        L.delay $ 1000 * 10000
        successorsRequest myAddress nodeData

    L.awaitNodeFinished nodeData