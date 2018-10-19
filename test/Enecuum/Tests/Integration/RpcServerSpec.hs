{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Tests.Integration.RpcServerSpec where

import           Enecuum.Prelude

import           Data.Aeson as A
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit ( fromHUnitTest )
import qualified Data.Map           as M

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import qualified Enecuum.Runtime as R

--import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Interpreters
import           Enecuum.Framework.Networking.Interpreter

createNodeRuntime = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= (\a -> R.createNodeRuntime a M.empty)

data OkRequest = OkRequest deriving (Show, Eq, Generic, ToJSON, FromJSON)
data OkResponse = OkResponse deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ErrRequest = ErrRequest deriving (Show, Eq, Generic, ToJSON, FromJSON)
data ErrResponse = ErrResponse deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Tests disabled
spec :: Spec
spec = describe "RpcServer" $ fromHUnitTest $ TestList
    [TestLabel "Test of rpc server/ok" rpcServerTestOk, TestLabel "Test of rpc server/err" rpcServerTestErr]

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

serverPort = 2001
localServer = D.Address "127.0.0.1" serverPort
