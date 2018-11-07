{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Nodes.TestClient (testClient, TestClient, TestClientData, NodeConfig(..)) where

import           Enecuum.Prelude
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Language               as L
import qualified Enecuum.Assets.Nodes.Address   as A
import qualified Enecuum.Assets.Nodes.Messages  as M
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Framework.Language.Extra (HasStatus)
import           Enecuum.Config
import qualified Data.Aeson                       as J
import           Enecuum.Assets.Nodes.Methods

newtype TestClientData = TestClientData
    { _status   :: D.StateVar L.NodeStatus
    }
makeFieldsNoPrefix ''TestClientData

data TestClient = TestClient
    deriving (Show, Generic)

data instance NodeConfig TestClient = TestClientConfig
    { _dummyOptionTestClient :: Int
    }
    deriving (Show, Generic)

instance Node TestClient where
    data NodeScenario TestClient = TestClientS
        deriving (Show, Generic)
    getNodeScript TestClientS = testClient

instance ToJSON   TestClient                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON TestClient                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TestClient)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TestClient)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario TestClient) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TestClient) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

testClient _ = do
    L.nodeTag "Client node"
    L.logInfo "Starting of test client node"
    let serverAddress = D.Address "127.0.0.1" 5000
    nodeData <- L.scenario $ L.atomically (TestClientData <$> L.newVar L.NodeActing)
    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    conn <- L.open D.Tcp serverAddress $ pure ()
    tester conn 0
    
    L.logInfo "Connect closed."
tester :: (L.Logger m, L.Send t m, L.ControlFlow m, Monad m) => t -> Int -> m ()
tester conn i = do
    L.delay i
    res <- L.send conn M.Ping
    L.logInfo $ show i
    when (isRight res) $ tester conn $ i + 1000000