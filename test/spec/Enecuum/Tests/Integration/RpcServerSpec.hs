{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Tests.Integration.RpcServerSpec where

import           Enecuum.Prelude

import qualified Data.Map                 as M
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Enecuum.Domain           as D
import           Enecuum.Interpreters
import qualified Enecuum.Language         as L
import qualified Enecuum.Runtime          as R
import           Enecuum.Tests.Helpers
import           Enecuum.Testing.Wrappers

createNodeRuntime :: IO R.NodeRuntime
createNodeRuntime = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= (`R.createNodeRuntime` M.empty)

data OkRequest = OkRequest deriving (Show, Eq, Generic, ToJSON, FromJSON)
data OkResponse = OkResponse deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ErrRequest = ErrRequest deriving (Show, Eq, Generic, ToJSON, FromJSON)
data ErrResponse = ErrResponse deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Tests disabled
spec :: Spec
spec = stableTest $ fastTest $ describe "RpcServer" $ fromHUnitTest $ TestList
    [ TestLabel "Test of rpc server/ok" rpcServerTestOk
    , TestLabel "Test of rpc server/err" rpcServerTestErr
    ]

okHandler :: OkRequest -> L.NodeL OkResponse
okHandler _ = pure OkResponse

errHandler :: ErrRequest -> L.NodeL ErrResponse
errHandler _ = pure ErrResponse

rpcServerTestOk :: Test
rpcServerTestOk = TestCase $ do
    nr <- createNodeRuntime
    threadDelay 10000
    runNodeDefinitionL nr $ L.serving D.Rpc serverPort $ do
        L.method okHandler
        L.method errHandler
    threadDelay 10000
    res <- runNodeL nr $ L.makeRpcRequest localServer OkRequest
    runNodeDefinitionL nr $ L.stopServing serverPort
    assertBool "" (res == Right OkResponse)


rpcServerTestErr :: Test
rpcServerTestErr = TestCase $ do
    nr <- createNodeRuntime
    threadDelay 10000
    runNodeDefinitionL nr $ L.serving D.Rpc serverPort $ do
        L.method okHandler
        L.method errHandler
    threadDelay 10000
    res <- runNodeL nr $ L.makeRpcRequest localServer ErrRequest
    runNodeDefinitionL nr $ L.stopServing serverPort
    assertBool "" (res == Right ErrResponse)

serverPort :: D.PortNumber
serverPort = 2001

localServer :: D.Address
localServer = D.Address "127.0.0.1" serverPort
