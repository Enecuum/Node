{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Framework.RpcServerSpec where

import           Enecuum.Prelude

import           Data.Aeson as A
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import qualified Enecuum.Runtime as Rt

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Interpreters
import qualified Enecuum.Framework.Domain.RpcMessages as R
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.Networking.Interpreter

createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= Rt.createNodeRuntime

data OkRequest = OkRequest deriving (Show, Eq, Generic, ToJSON, FromJSON)
data OkResponse = OkResponse deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ErrRequest = ErrRequest deriving (Show, Eq, Generic, ToJSON, FromJSON)
data ErrResponse = ErrResponse deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Tests disabled
spec :: Spec
spec = describe "RpcServer" $ fromHUnitTest $ TestList
    [ TestLabel "Test of rpc server/ok" rpcServerTestOk
    , TestLabel "Test of rpc server/err" rpcServerTestErr
    ]

okHandler :: OkRequest -> NodeL OkResponse
okHandler _ = pure OkResponse

errHandler :: ErrRequest -> NodeL ErrResponse
errHandler _ = pure ErrResponse

rpcServerTestOk :: Test
rpcServerTestOk = TestCase $ do
    nr <- createNodeRuntime
    runNodeDefinitionL nr $ servingRpc serverPort $ do
        method okHandler
        method errHandler
    threadDelay 1000
    res <- runNodeL nr $ makeRpcRequest localServer OkRequest
    runNodeDefinitionL nr $ stopServing serverPort
    assertBool "" (res == Right OkResponse)


rpcServerTestErr :: Test
rpcServerTestErr = TestCase $ do
    nr <- createNodeRuntime
    runNodeDefinitionL nr $ servingRpc serverPort $ do
        method okHandler
        method errHandler
    threadDelay 1000
    res <- runNodeL nr $ makeRpcRequest localServer ErrRequest
    runNodeDefinitionL nr $ stopServing serverPort
    assertBool "" (res == Right ErrResponse)

serverPort  = 1666
localServer = D.Address "127.0.0.1" serverPort
