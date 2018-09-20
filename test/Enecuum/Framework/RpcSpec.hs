module Enecuum.Framework.RpcSpec where

import           Enecuum.Prelude

import           Data.Aeson as A
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Interpreters
import           Enecuum.Language
import qualified Enecuum.Framework.Domain.RpcMessages as R
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
    nr <- createNodeRuntime
    runNodeDefinitionL nr $ servingRpc 1666 serverMethodes
    threadDelay 1000
    Right (R.RpcResponseResult res _) <- runNetworkingL $
        sendRpcRequest (ConnectInfo "127.0.0.1" 1666) (R.RpcRequest "ok" (A.String "") 1)
    assertBool "" (res == A.String "Ok")


rpcServerTestErr :: Test
rpcServerTestErr = TestCase $ do
    nr <- createNodeRuntime
    runNodeDefinitionL nr $ servingRpc 1667 serverMethodes
    threadDelay 1000
    Right (R.RpcResponseError res _) <- runNetworkingL $
        sendRpcRequest  (ConnectInfo "127.0.0.1" 1667) (R.RpcRequest "error" (A.String "") 1)
    assertBool "" (res == A.String ":(")
