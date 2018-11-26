{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.BN (bnNode, BN, NodeConfig(..)) where

import qualified Data.Aeson                            as J
import qualified Enecuum.Assets.Nodes.Address          as A
import qualified Enecuum.Assets.Nodes.Messages         as M
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.Routing.Messages
import           Enecuum.Config
import qualified Enecuum.Domain                        as D
import           Enecuum.Framework.Language.Extra      (HasStatus)
import qualified Enecuum.Framework.Lens                as Lens
import qualified Enecuum.Language                      as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap

data BNNodeData = BNNodeData
    { _status   :: D.StateVar D.NodeStatus
    , _netNodes :: D.StateVar (ChordRouteMap D.NodeAddress)
    }

data BN = BN
    deriving (Show, Generic)

data instance NodeConfig BN = BNConfig
    { _dummyOptionBN :: Int
    }
    deriving (Show, Generic)

instance Node BN where
    data NodeScenario BN = BNS
        deriving (Show, Generic)
    getNodeScript BNS = bnNode'

instance ToJSON   BN                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON BN                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig BN)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig BN)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario BN) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario BN) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

initBN :: L.NodeDefinitionL BNNodeData
initBN = L.atomically (BNNodeData <$> L.newVar D.NodeActing <*> L.newVar mempty)

-- TODO add identification of host address.
acceptNewNode :: BNNodeData -> HelloToBn -> D.Connection D.Udp -> L.NodeL ()
acceptNewNode nodeData helloToBn conn = L.close conn >> do
    let host    = D.getHostAddress conn
    let nId     = helloToBn ^. senderId
    let ports   = helloToBn ^. senderPorts
    when (verifyHelloToBn helloToBn) $ do
        let address = A.makeNodeAddress host ports nId
        L.logInfo $ "New node is accepted. " <> show address
        L.modifyVarIO (_netNodes nodeData) $ addToMap nId address

findConnect :: BNNodeData -> M.ConnectRequest -> L.NodeL (Either Text (D.StringHash, D.NodeAddress))
findConnect nodeData (M.ConnectRequest hash i) = do
    address <- L.atomically $ do
        connectMap <- L.readVar (_netNodes nodeData)
        pure $ findInMapNByKey
            (\h j -> D.hashToWord64 h + 2 ^ j)
            i
            hash
            connectMap
    pure $ maybe (Left "Connection map is empty.") Right address

findNextConnectForMe :: BNNodeData -> M.NextForMe -> L.NodeL (Either Text (D.StringHash, D.NodeAddress))
findNextConnectForMe nodeData (M.NextForMe hash) = do
    address <- L.atomically $ do
        connectMap <- L.readVar (_netNodes nodeData)
        pure $ findNextForHash hash connectMap
    pure $ maybe (Left "Connection map is empty.") Right address

bnNode :: L.NodeDefinitionL ()
bnNode = bnNode' $ BNConfig 42

isDeadAccept :: BNNodeData -> M.IsDead -> D.Connection D.Udp -> L.NodeL ()
isDeadAccept nodeData (M.IsDead hash) connect = do
    L.close connect
    connectMap <- L.readVarIO (_netNodes nodeData)
    let mDeadNodeAddress = findConnectByHash hash connectMap
    whenJust mDeadNodeAddress $ \address -> do
        res :: Either Text M.Pong <- L.makeRpcRequest (A.getRpcAddress address) M.Ping
        when (isLeft res) $ L.atomically $ L.modifyVar (_netNodes nodeData) $ removeFromMap hash

acceptAddressRequest :: BNNodeData -> AddressRequest -> L.NodeL (Either Text D.NodeAddress)
acceptAddressRequest nodeData (AddressRequest nodeLogicAddress) = do
    connectMap <- L.readVarIO (_netNodes nodeData)
    let eAddress = getByHash nodeLogicAddress connectMap
    pure $ case eAddress of
        Just address -> Right address
        Nothing      -> Left "The node doesn't exist in route map."

bnNode' :: NodeConfig BN -> L.NodeDefinitionL ()
bnNode' _ = do
    L.nodeTag "BN node"
    L.logInfo "Starting of BN node"
    nodeData <- initBN
    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    let bnPorts = A.defaultBnNodePorts
    -- TODO  Process serving error (it's ignored now).
    void $ L.serving D.Udp (bnPorts ^. Lens.nodeUdpPort) $ do
        L.handler $ isDeadAccept  nodeData
        L.handler $ acceptNewNode nodeData

    void $ L.serving D.Rpc (bnPorts ^. Lens.nodeRpcPort) $ do
        -- network
        L.method  $ handleStopNode       nodeData
        L.methodE $ findConnect          nodeData
        L.methodE $ acceptAddressRequest nodeData
        -- clockwise direction
        L.methodE $ findNextConnectForMe nodeData

    L.awaitNodeFinished nodeData

makeFieldsNoPrefix ''BNNodeData
