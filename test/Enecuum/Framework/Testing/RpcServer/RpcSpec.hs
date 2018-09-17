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
import           Enecuum.Core.Language                    ( CoreEffects )
import           Enecuum.Framework.Networking.Interpreter


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
    Just (R.RpcResponseResult res _) <- runSafeIO $ startNetworkingModel $
        sendRpcRequest localServer (R.RpcRequest "ok" (A.String "") 1)
    assertBool "" (res == A.String "Ok")


rpcServerTestErr :: Test
rpcServerTestErr = TestCase $ do
    threadDelay 10000
    Just (R.RpcResponseError res _) <- runSafeIO $ startNetworkingModel $
        sendRpcRequest localServer (R.RpcRequest "error" (A.String "") 1)
    assertBool "" (res == A.String ":(")

localServer = ConnectInfo "127.0.0.1" 1666

startNetworkingModel
  :: Eff ('[NetworkingL, NetworkSyncL, NetworkListeningL, NetworkSendingL] ++ CoreEffects) a
  -> Eff '[SIO, Exc SomeException] a
startNetworkingModel
    = handleRelay pure ( (>>=) . interpretLoggerL)
    . handleRelay pure ( (>>=) . interpretNetworkSendingL)
    . handleRelay pure ( (>>=) . interpretNetworkListeningL)
    . handleRelay pure ( (>>=) . interpretNetworkSyncL)
    . handleRelay pure ( (>>=) . interpretNetworkingL)

