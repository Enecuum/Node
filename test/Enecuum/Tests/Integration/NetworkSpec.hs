{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass        #-}
module Enecuum.Tests.Integration.NetworkSpec where

--

import           Enecuum.Prelude

import           Control.Concurrent.MVar (isEmptyMVar)
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )
import qualified Enecuum.Language              as L

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Interpreters
import qualified Enecuum.Runtime as Rt
import qualified Enecuum.Domain                as D
import           Enecuum.Framework.Networking.Internal.Udp.Connection
import           Enecuum.Framework.Networking.Internal.Udp.Server
import           Enecuum.Framework.Networking.Internal.Client
import qualified Data.Map as M
import qualified Network.Socket.ByteString.Lazy as S
import           Control.Concurrent.STM.TChan
import qualified Enecuum.Framework.Networking.Internal.Connection as Con

-- Tests disabled
spec :: Spec
spec = describe "Network tests" $ fromHUnitTest $ TestList
    [ TestLabel "udp ping-pong test (successful sending udp msg by connect & address )" (pingPongTest                     D.Udp 4000 5000)
    , TestLabel "tcp ping-pong test"                                                    (pingPongTest                     D.Tcp 4001 5001)
    , TestLabel "fail sending too big msg by udp connect"                               (testSendingBigMsgByConnect       D.Udp 4002 5002)
    , TestLabel "fail sending too big msg by tcp connect"                               (testSendingBigMsgByConnect       D.Tcp 4003 5003)
    , TestLabel "fail sending too big udp msg by Address"                               (testSendingBigUdpMsgByAddress          4004 5004)
    , TestLabel "fail sending msg by closed udp connect"                                (testSendingMsgToClosedConnection D.Udp 4005 5005)
    , TestLabel "fail sending msg by closed tcp connect"                                (testSendingMsgToClosedConnection D.Tcp 4006 5006)
  --  This functionality is not supported!
  --, TestLabel "fail sending udp msg to nonexistent address"                           (testSendingMsgToNonexistentAddress         5007)
  --, TestLabel "fail udp connecting to nonexistent address."                           (testConnectToNonexistentAddress D.Udp      5008)
    , TestLabel "fail tcp connecting to nonexistent address."                           (testConnectToNonexistentAddress D.Tcp      5009)
  --  This functionality is not supported!
  --, TestLabel "fail udp connecting to tcp server."                                    (testConnectFromTo         D.Tcp D.Udp 4010 5010)
    , TestLabel "fail tcp connecting to udp server."                                    (testConnectFromTo         D.Udp D.Tcp 4011 5011)
    ]

newtype Ping   = Ping Int deriving (Generic, ToJSON, FromJSON)
newtype Pong   = Pong Int deriving (Generic, ToJSON, FromJSON)
newtype BigMsg = BigMsg [Int] deriving (Generic, ToJSON, FromJSON)
data Success    = Success   deriving (Generic, ToJSON, FromJSON)

bigMsg = BigMsg [1..5000]

-- TODO tests:
-- Fail. Udp connecting to nonexistent address. [-]
-- Fail. Tcp connecting to nonexistent address. [+]
-- Fail. Udp connect to Tcp server.             [+]
-- Fail. Tcp connect to Udp server.             [-]
-- Fail. Sending udp msg to nonexistent address.[-]
-- Fail. Sending udp msg to closed connection.  [+]
-- Fail. Sending tcp msg to closed connection.  [+]
-- Fail. Sending too big msg by tcp.            [+]
-- Fail. Sending too big msg by udp connection. [+]
-- Fail. Sending too big msg by udp address.    [+]
-- Successful. Sending by udp connect.          [+]
-- Successful. Sending by tcp connect.          [+]
-- Successful. Sending udp msg by address.      [+]

createNodeRuntime :: IO Rt.NodeRuntime
createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= Rt.createNodeRuntime

testConnectFromTo prot1 prot2 serverPort succPort = do
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $ do
            L.serving prot1 serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn                 <- L.open prot2 serverAddr $ pure ()
            Left D.ConnectionClosed <- L.send conn $ Success
            void $ L.notify succAddr Success


testConnectToNonexistentAddress prot succPort = do
    runServingScenarion succPort succPort $ \_ succAddr nodeRt1 _ -> do
        runNodeDefinitionL nodeRt1 $ do
            conn                 <- L.open prot (D.Address "127.0.0.1" 300) $ pure ()
            Left D.ConnectionClosed <- L.send conn Success
            void $ L.notify succAddr Success

testSendingMsgToNonexistentAddress succPort = do
    runServingScenarion succPort succPort $ \_ succAddr nodeRt1 _ -> do
        runNodeDefinitionL nodeRt1 $ do
            Left D.AddressNotExist <- L.notify (D.Address "127.0.0.1" 300) Success
            void $ L.notify succAddr Success

testSendingMsgToClosedConnection prot serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $ do
            L.serving prot serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn                 <- L.open prot serverAddr $ pure ()
            L.close conn
            Left D.ConnectionClosed <- L.send conn $ Success
            void $ L.notify succAddr Success

testSendingBigMsgByConnect prot serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $ do
            L.serving prot serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn             <- L.open prot serverAddr $ pure ()
            Left D.TooBigMessage <- L.send conn $ bigMsg
            void $ L.notify succAddr Success

testSendingBigUdpMsgByAddress serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $ do
            L.serving D.Tcp serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            Left _ <- L.notify serverAddr $ bigMsg
            void $ L.notify succAddr Success

pingPongTest prot serverPort succPort = 
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $ do
            L.serving prot serverPort $ do
                L.handler (pingHandle succAddr)
                L.handler (pongHandle succAddr)
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn <- L.open prot serverAddr $ do
                L.handler (pingHandle succAddr)
                L.handler (pongHandle succAddr)
            void $ L.send conn $ Ping 0


runServingScenarion serverPort succPort f = TestCase $ do
    let serverAddr = D.Address "127.0.0.1" serverPort
        succAddr    = D.Address "127.0.0.1" succPort
    nodeRt1 <- createNodeRuntime
    nodeRt2 <- createNodeRuntime
    void $ forkIO $ f serverAddr succAddr nodeRt1 nodeRt2
    ok <- succesServer succPort
    runNodeDefinitionL nodeRt1 $ L.stopServing serverPort
    assertBool "" ok    

pingHandle succAddr (Ping i) conn = do
    when (i < 10) $ void $ L.send conn (Pong $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAddr Success
        L.close conn

pongHandle succAddr (Pong i) conn = do
    when (i < 10) $ void $ L.send conn (Ping $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAddr Success
        L.close conn

emptFunc :: D.Connection D.Udp -> D.ConnectionVar D.Udp -> IO ()
emptFunc _ _ = pure ()

succesServer :: PortNumber -> IO Bool
succesServer port = do
    mvar <- newEmptyMVar
    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar False
    ch <- Con.startServer port (M.singleton (D.toTag Success) (\_ _ -> putMVar mvar True)) emptFunc putTextLn
    ok <- takeMVar mvar
    Enecuum.Prelude.atomically $ Con.stopServer ch
    pure ok