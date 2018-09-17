module Enecuum.Framework.Testing.RpcServer.RpcSpec where

import           Enecuum.Prelude
import           Test.HUnit
import           Data.Aeson as A
import           Enecuum.Core.Logger.Interpreter
import           Enecuum.Framework.RpcMethod.Language
import           Enecuum.Framework.NodeDefinition.Interpreter
import           Enecuum.Framework.NodeDefinition.Language
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import qualified Network.WebSockets                               as WS

spec :: Spec
spec = describe "RpcServer" $ fromHUnitTest $ TestList
    [ TestLabel "start of rpc" rpcServerStart
    , TestLabel "Test of rpc server/ok" rpcServerTestOk
    , TestLabel "Test of rpc server/err" rpcServerTestErr
    ]

serverMethodes = do
    rpcMethod "ok"    (\_ i -> return $ RpcResponseResult (A.String "Ok") i)
    rpcMethod "error" (\_ i -> return $ RpcResponseError (A.String ":(") i)

rpcServerStart :: Test
rpcServerStart = TestCase $ do
    runSafeIO $ runLoggerL $ runNodeDefinitionL $ servingRpc $ serverMethodes
    assertBool "" True

rpcServerTestOk :: Test
rpcServerTestOk = TestCase $ do
    threadDelay 10000
    runClient "127.0.0.1" 1666 "/" $ \connect -> do
        WS.sendTextData connect $ A.encode $ RpcRequest "ok" (A.String "") 1
        msg <- WS.receiveData connect
        let Just (RpcResponseResult res _) = A.decode msg
        assertBool "" (res == A.String "Ok")

rpcServerTestErr :: Test
rpcServerTestErr = TestCase $ do
    threadDelay 10000
    runClient "127.0.0.1" 1666 "/" $ \connect -> do
        WS.sendTextData connect $ A.encode $ RpcRequest "error" (A.String "") 1
        msg <- WS.receiveData connect
        let Just (RpcResponseError res _) = A.decode msg
        assertBool "" (res == A.String ":(")