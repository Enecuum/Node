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
import qualified Enecuum.Framework.Domain.RpcMessages as R
import           Eff                                ( handleRelay )
import           Enecuum.Framework.NetworkModel.Interpreter
import           Enecuum.Framework.Networking.Language
import           Enecuum.Framework.NetworkModel.Language
import           Enecuum.Framework.Networking.Interpreter
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.Node.Runtime


spec :: Spec
spec = describe "RpcServer" $ fromHUnitTest $ TestList
    [ TestLabel "Test of rpc server/ok" rpcServerTestOk
    , TestLabel "Test of rpc server/err" rpcServerTestErr
    ]

serverMethodes = do
    rpcMethod "ok"    (\_ i -> return $ RpcResponseResult (A.String "Ok") i)
    rpcMethod "error" (\_ i -> return $ RpcResponseError (A.String ":(") i)


rpcServerTestOk :: Test
rpcServerTestOk = TestCase $ do
    nr <- makeNodeRuntime
    runNodeDefinitionL nr $ servingRpc 1666 serverMethodes
    threadDelay 1000
    Right (R.RpcResponseResult res _) <- runNetworkingL $
        sendRpcRequest (ConnectInfo "127.0.0.1" 1666) (R.RpcRequest "ok" (A.String "") 1)
    assertBool "" (res == A.String "Ok")


rpcServerTestErr :: Test
rpcServerTestErr = TestCase $ do
    nr <- makeNodeRuntime
    runNodeDefinitionL nr $ servingRpc 1667 serverMethodes
    threadDelay 1000
    Right (R.RpcResponseError res _) <- runNetworkingL $
        sendRpcRequest  (ConnectInfo "127.0.0.1" 1667) (R.RpcRequest "error" (A.String "") 1)
    assertBool "" (res == A.String ":(")
