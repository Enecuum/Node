module Enecuum.Tests.Scenarios.ForkSpec where

import qualified Enecuum.Assets.Scenarios      as A
import qualified Enecuum.Domain                as D
import           Enecuum.Prelude
import           Enecuum.Testing.Integrational
import           Enecuum.Tests.Wrappers
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = slowTest $ describe "Test Forks" $ fromHUnitTest $ TestList
    -- [ ]
    [TestLabel "test Forks " testForks]

testForks = TestCase $ withNodesManager $ \mgr -> do
    let graphNodeConfig1 = A.defaultNodeConfig { A._udpPort = 4001
                                               , A._tcpPort = 4002
                                               , A._rpcPort = 4003
                                               }
    let graphNodeConfig2 = A.defaultNodeConfig { A._udpPort = 5001
                                               , A._tcpPort = 5002
                                               , A._rpcPort = 5003
                                               }
    void $ startNode Nothing mgr $ A.graphNodeTransmitter graphNodeConfig1
    threadDelay $ 1000 * 2000
    void $ startNode Nothing mgr $ A.graphNodeTransmitter graphNodeConfig2
    -- void $ startNode Nothing mgr $ A.graphNodeReceiver graphNodeConfig2

    -- waitForNode $ D.Address A.localhost 4003
    -- waitForNode $ D.Address A.localhost 5003    
    let udp1 = D.Address A.localhost 4001
    let udp2 = D.Address A.localhost 5001    
    void $ startNode Nothing mgr $ A.powNode' A.defaultPoWNodeConfig { A._graphNodeUDPAddress = udp1}
    -- void $ startNode Nothing mgr $ A.powNode' A.defaultPoWNodeConfig { A._graphNodeUDPAddress = udp2}          


    True `shouldBe` True
