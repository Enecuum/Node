{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Nodes.TestServer (testServer, TestServer, NodeConfig(..)) where

import           Enecuum.Prelude
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Language               as L
import           Enecuum.Framework.Language.Extra (HasStatus)
import           Enecuum.Config
import qualified Data.Aeson                       as J


newtype TestServerData = TestServerData
    { _status   :: D.StateVar L.NodeStatus
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
    nodeData <- L.scenario $ L.atomically (TestServerData <$> L.newVar L.NodeActing)
    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.serving D.Tcp 5000 $ pure ()
    L.awaitNodeFinished nodeData