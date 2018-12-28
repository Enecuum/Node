
module Enecuum.Tests.Functional.RetryAndTcpLikeSpec where

import Enecuum.Prelude

import           Test.Hspec
import           Test.HUnit
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )

import           Enecuum.TestData.Nodes.Scenarios
import           Enecuum.Testing
import qualified Enecuum.Testing.RLens as RLens
import           Enecuum.Testing.Wrappers

-- Tests disabled
spec :: Spec
spec = stableTest $ fastTest $ describe "Retry & TCP-like connections test" $ fromHUnitTest $ TestList
    [TestLabel "Retry & TCP-like connections test (Ping-Pong 2)" pingPong2]

pingPong2 :: Test
pingPong2 = TestCase $ do
    runtime <- createTestRuntime

    void $ forkIO $ void $ startNode runtime pongServerAddress pongServingNode
    threadDelay $ 1000 * 1000
    void $ startNode runtime pingClientAddress pingSendingClientNode
    threadDelay $ 1000 * 1000

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldContain` ["Pong handle received: Pong {pong = 3}. Sending Ping {ping = 4}."]
    msgs `shouldContain` ["Ping handle received: Ping {ping = 3}. Sending Pong {pong = 3}."]
    msgs `shouldContain` ["Pong handle received: Pong {pong = 2}. Sending Ping {ping = 3}."]
    msgs `shouldContain` ["Ping handle received: Ping {ping = 2}. Sending Pong {pong = 2}."]
    msgs `shouldContain` ["Pong handle received: Pong {pong = 1}. Sending Ping {ping = 2}."]
    msgs `shouldContain` ["Ping handle received: Ping {ping = 1}. Sending Pong {pong = 1}."]
    msgs `shouldContain` ["Pong handle received: Pong {pong = 0}. Sending Ping {ping = 1}."]
    msgs `shouldContain` ["Ping handle received: Ping {ping = 0}. Sending Pong {pong = 0}."]
