{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.TestServer (testServer, TestServer, NodeConfig(..)) where

import qualified Data.Aeson                       as J
import           Enecuum.Config
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasStatus)
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude


newtype TestServerData = TestServerData
    { _status   :: D.StateVar D.NodeStatus
    }
makeFieldsNoPrefix ''TestServerData

data TestServer = TestServer
    deriving (Show, Generic)

data instance NodeConfig TestServer = TestServerConfig
    { _dummyOptionTestServer :: Int
    }
    deriving (Show, Generic)

instance Node TestServer where
    data NodeScenario TestServer = TestServerS
        deriving (Show, Generic)
    getNodeScript TestServerS = testServer
    getNodeTag _ = TestServer

instance ToJSON   TestServer                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON TestServer                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TestServer)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TestServer)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario TestServer) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TestServer) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

testServer :: p -> L.NodeDefinitionL ()
testServer _ = do
    L.nodeTag "Server node"
    L.logInfo "Starting of test server node"
    nodeData <- L.scenario $ L.atomically (TestServerData <$> L.newVar D.NodeActing)
    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.serving D.Tcp 5000 $ pure ()
    L.awaitNodeFinished nodeData
