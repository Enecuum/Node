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
    [ TestLabel "udp ping-pong test (successful sending udp msg by connect & addres )"  (pingPongTest                     D.Udp 4000 5000)
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
data Succes    = Succes   deriving (Generic, ToJSON, FromJSON)

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
-- Fail. Sending too big msg by udp addres.     [+]
-- Successful. Sending by udp connet.           [+]
-- Successful. Sending by tcp connet.           [+]
-- Successful. Sending udp msg by addres.       [+]

createNodeRuntime :: IO Rt.NodeRuntime
createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= Rt.createNodeRuntime

testConnectFromTo prot1 prot2 serverPort succPort = do
    runServingScenarion serverPort succPort $ \serverAddr succAdr nr1 nr2 -> do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            L.serving prot1 serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nr2 $ do
            conn                             <- L.open prot2 serverAddr $ pure ()
            Left "The connection is closed." <- L.send conn $ Succes
            void $ L.notify succAdr Succes


testConnectToNonexistentAddress prot succPort = do
    runServingScenarion succPort succPort $ \_ succAdr nr1 _ -> do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            conn                             <- L.open prot (D.Address "127.0.0.1" 300) $ pure ()
            Left "The connection is closed." <- L.send conn Succes
            void $ L.notify succAdr Succes

testSendingMsgToNonexistentAddress succPort = do
    runServingScenarion succPort succPort $ \_ succAdr nr1 _ -> do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            Left "The address does not exist." <- L.notify (D.Address "127.0.0.1" 300) Succes
            void $ L.notify succAdr Succes

testSendingMsgToClosedConnection prot serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAdr nr1 nr2 -> do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            L.serving prot serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nr2 $ do
            conn                             <- L.open prot serverAddr $ pure ()
            L.close conn
            Left "The connection is closed." <- L.send conn $ Succes
            void $ L.notify succAdr Succes

testSendingBigMsgByConnect prot serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAdr nr1 nr2 -> do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            L.serving prot serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nr2 $ do
            conn                           <- L.open prot serverAddr $ pure ()
            Left "The message is too big." <- L.send conn $ bigMsg
            void $ L.notify succAdr Succes

testSendingBigUdpMsgByAddress serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAdr nr1 nr2 -> do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            L.serving D.Tcp serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nr2 $ do
            Left _ <- L.notify serverAddr $ bigMsg
            void $ L.notify succAdr Succes

pingPongTest prot serverPort succPort = 
    runServingScenarion serverPort succPort $ \serverAddr succAdr nr1 nr2 -> do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            L.serving prot serverPort $ do
                L.handler (pingHandle succAdr)
                L.handler (pongHandle succAdr)
        
        threadDelay 5000
        runNodeDefinitionL nr2 $ do
            conn <- L.open prot serverAddr $ do
                L.handler (pingHandle succAdr)
                L.handler (pongHandle succAdr)
            void $ L.send conn $ Ping 0


runServingScenarion serverPort succPort f = TestCase $ do
    let serverAddr = D.Address "127.0.0.1" serverPort
        succAdr    = D.Address "127.0.0.1" succPort
    nr1 <- createNodeRuntime
    nr2 <- createNodeRuntime
    void $ forkIO $ f serverAddr succAdr nr1 nr2
    ok <- succesServer succPort
    runNodeDefinitionL nr1 $ L.stopServing serverPort
    assertBool "" ok    

pingHandle succAdr (Ping i) conn = do
    when (i < 10) $ void $ L.send conn (Pong $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAdr Succes
        L.close conn

pongHandle succAdr (Pong i) conn = do
    when (i < 10) $ void $ L.send conn (Ping $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAdr Succes
        L.close conn

emptFunc :: D.Connection D.Udp -> D.ConnectionVar D.Udp -> IO ()
emptFunc _ _ = pure ()

succesServer :: PortNumber -> IO Bool
succesServer port = do
    mvar <- newEmptyMVar
    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar False
    ch <- Con.startServer port (M.singleton (D.toTag Succes) (\_ _ -> putMVar mvar True)) emptFunc putTextLn
    ok <- takeMVar mvar
    Enecuum.Prelude.atomically $ Con.stopServer ch
    pure ok
