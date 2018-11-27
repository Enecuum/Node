{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.TestClient (testClient, TestClient, TestClientData, NodeConfig(..)) where

import qualified Data.Aeson                       as J
import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Config
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasStatus)
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap

newtype TestClientData = TestClientData
    { _status   :: D.StateVar D.NodeStatus
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
    getNodeTag _ = TestClient 

instance ToJSON   TestClient                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON TestClient                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TestClient)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TestClient)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario TestClient) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TestClient) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

testClient :: p -> Free L.NodeDefinitionF ()
testClient _ = do
    L.nodeTag "Client node"
    L.logInfo "Starting of test client node"
    let serverAddress = D.Address "127.0.0.1" 5000
    nodeData <- L.atomically (TestClientData <$> L.newVar D.NodeActing)
    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    conn <- L.open D.Tcp serverAddress $ pure ()
    whenJust conn $ \jConn -> tester jConn 0
    L.logInfo "Connect closed."

tester :: (L.Logger m, L.Send t m, L.ControlFlow m, Monad m) => t -> Int -> m ()
tester conn i = do
    L.delay i
    res <- L.send conn M.Ping
    L.logInfo $ show i
    when (isRight res) $ tester conn $ i + 1000000
