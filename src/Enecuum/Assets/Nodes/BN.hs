{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Nodes.BN (bnNode, BN) where

import           Enecuum.Prelude
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Language               as L
import qualified Enecuum.Assets.Nodes.Address   as A
import qualified Enecuum.Assets.Nodes.Messages  as M
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Framework.Language.Extra (HasStatus)
import           Enecuum.Assets.Nodes.Methods (methodStopNode)
import           Enecuum.Config
import qualified Data.Aeson                       as J


data BNNodeData = BNNodeData
    { _status   :: D.StateVar L.NodeStatus
    , _netNodes :: D.StateVar (ChordRouteMap D.Address)
    }
makeFieldsNoPrefix ''BNNodeData

data BN = BN
    deriving (Show, Generic)

data instance NodeConfig BN = BNConfig
    { dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node BN where
    data NodeScenario BN = BNS
        deriving (Show, Generic)
    getNodeScript BNS = bnNode

instance ToJSON   BN                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON BN                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig BN)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig BN)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario BN) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario BN) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

initBN :: L.NodeDefinitionL BNNodeData
initBN = L.atomically (BNNodeData <$> L.newVar L.NodeActing <*> L.newVar mempty)

-- TODO add identification of host address.
acceptNewNode :: BNNodeData -> M.Hello -> L.NodeL M.SuccessMsg
acceptNewNode nodeData (M.Hello hash address) = do
    void $ L.atomically $
        L.modifyVar (nodeData ^. netNodes) $ addToMap hash address
    pure M.SuccessMsg

findPreviousConnectForMe :: BNNodeData -> M.ConnectRequestPrevious -> L.NodeL (Either Text (D.StringHash, D.Address))
findPreviousConnectForMe nodeData (M.ConnectRequestPrevious hash) = do
    address <- L.atomically $ do
        connectMap <- L.readVar (nodeData ^. netNodes)
        pure $ findInMapNByKey
            inverseFormula
            0
            hash
            connectMap
    pure $ maybe (Left "Connection map is empty.") Right address

findConnect :: BNNodeData -> M.ConnectRequest -> L.NodeL (Either Text (D.StringHash, D.Address))
findConnect nodeData (M.ConnectRequest hash i) = do
    address <- L.atomically $ do
        connectMap <- L.readVar (nodeData ^. netNodes)
        pure $ findInMapNByKey
            straightFormula
            i
            hash
            connectMap
    pure $ maybe (Left "Connection map is empty.") Right address


findNextConnectForMe :: BNNodeData -> M.NextForMe -> L.NodeL (Either Text (D.StringHash, D.Address))
findNextConnectForMe nodeData (M.NextForMe hash) = do
    address <- L.atomically $ do
        connectMap <- L.readVar (nodeData ^. netNodes)
        pure $ findNextForHash hash connectMap
    pure $ maybe (Left "Connection map is empty.") Right address


bnNode :: NodeConfig BN -> L.NodeDefinitionL ()
bnNode _ = do
    L.nodeTag "BN node"
    L.logInfo "Starting of BN node"
    nodeData <- initBN
    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.serving D.Rpc A.bnNodePort $ do
        L.method  $ methodStopNode nodeData

        -- routing
        L.method  $ acceptNewNode       nodeData
        L.methodE $ findConnect         nodeData
        --  counterclockwise direction
        L.methodE $ findPreviousConnectForMe nodeData
        -- clockwise direction
        L.methodE $ findNextConnectForMe       nodeData

    L.awaitNodeFinished nodeData
