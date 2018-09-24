{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Framework.RpcServerSpec where

import           Enecuum.Prelude

import           Data.Aeson as A
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )
import qualified Enecuum.Language              as L

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Interpreters
import           Enecuum.Language
import qualified Enecuum.Framework.Domain.RpcMessages as R
import           Enecuum.Framework.Domain.RpcMessages
import qualified Enecuum.Runtime as Rt
import           Enecuum.Framework.Node.Language          ( NodeL )
import qualified Enecuum.Domain                as D
import           Enecuum.Framework.Networking.Interpreter
import           Enecuum.Framework.Environment

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

okHandler :: OkRequest -> NodeL cfg OkResponse
okHandler _ = pure OkResponse

errHandler :: ErrRequest -> NodeL cfg ErrResponse
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
localServer = D.ConnectionConfig (ConnectInfo "127.0.0.1" serverPort)




makeRpcRequest
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> NodeL RealWorld (Either Text b)
makeRpcRequest connectCfg arg = L.evalNetworking $ L.makeRpcRequest_ connectCfg arg


makeRequestUnsafe
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> NodeL RealWorld b
makeRequestUnsafe connectCfg arg =
    (\(Right a) -> a) <$> makeRpcRequest connectCfg arg
