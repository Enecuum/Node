{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass        #-}
module Enecuum.Tests.Integration.NetworkSpec where

--

import           Enecuum.Prelude

import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )
import qualified Enecuum.Language              as L


import           Enecuum.Interpreters
import qualified Enecuum.Runtime as Rt
import qualified Enecuum.Domain                as D
import qualified Data.Map as M
import qualified Enecuum.Framework.Networking.Internal.Connection as Con

-- Tests disabled
spec :: Spec
spec = describe "Network tests" $ fromHUnitTest $ TestList
    [ TestLabel "udp ping-pong test (successful sending udp msg by connect & address)"  (pingPongTest                     D.Udp 4000 5000)
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
    , TestLabel "release of udp serving resources"                                      (testReleaseOfResources          D.Udp 4012 5012)
    , TestLabel "release of tcp serving resources"                                      (testReleaseOfResources          D.Tcp 4013 5013)
    ]

newtype Ping   = Ping Int deriving (Generic, ToJSON, FromJSON)
newtype Pong   = Pong Int deriving (Generic, ToJSON, FromJSON)
newtype BigMsg = BigMsg [Int] deriving (Generic, ToJSON, FromJSON)
data Success   = Success   deriving (Generic, ToJSON, FromJSON)

bigMsg :: BigMsg
bigMsg = BigMsg [1..5000]


createNodeRuntime :: IO Rt.NodeRuntime
createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= (\a -> Rt.createNodeRuntime a M.empty)

--
testReleaseOfResources :: (Applicative f, L.Serving c (f ())) =>
                                c -> D.PortNumber -> D.PortNumber -> Test
testReleaseOfResources protocol serverPort succPort =
    runServingScenarion serverPort succPort $ \_ succAddr nodeRt1 _ ->
        runNodeDefinitionL nodeRt1 $ do
            L.serving protocol serverPort $ pure ()
            L.stopServing serverPort
            L.delay 5000
            L.serving protocol serverPort $ pure ()
            L.stopServing serverPort
            L.delay 5000
            L.serving protocol serverPort $ pure ()
            void $ L.notify succAddr Success

testConnectFromTo :: (L.Send
            (D.Connection con) (Free L.NetworkingF),
          L.Connection (Free L.NodeF) con, Applicative f,
          L.Serving c (f ())) =>
         c -> con -> D.PortNumber -> D.PortNumber -> Test

testConnectFromTo prot1 prot2 serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $
            L.serving prot1 serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn <- L.open prot2 serverAddr $ pure ()
            res  <- L.send conn Success
            when (Left D.ConnectionClosed == res) $
                void $ L.notify succAddr Success

testConnectToNonexistentAddress :: (L.Send
    (D.Connection con) (Free L.NetworkingF),
              L.Connection (Free L.NodeF) con) =>
             con -> D.PortNumber -> Test

testConnectToNonexistentAddress protocol succPort =
    runServingScenarion succPort succPort $ \_ succAddr nodeRt1 _ ->
        runNodeDefinitionL nodeRt1 $ do
            conn <- L.open protocol (D.Address "127.0.0.1" 300) $ pure ()
            res  <- L.send conn Success
            when (Left D.ConnectionClosed == res) $
                void $ L.notify succAddr Success

testSendingMsgToNonexistentAddress :: D.PortNumber -> Test
testSendingMsgToNonexistentAddress succPort =
    runServingScenarion succPort succPort $ \_ succAddr nodeRt1 _ ->
        runNodeDefinitionL nodeRt1 $ do
            res <- L.notify (D.Address "127.0.0.1" 300) Success
            when (Left D.AddressNotExist == res) $
                void $ L.notify succAddr Success

testSendingMsgToClosedConnection :: (L.Send
                (D.Connection con) (Free L.NetworkingF),
              L.Connection (Free L.NodeF) con, Applicative f,
              L.Serving con (f ())) =>
             con -> D.PortNumber -> D.PortNumber -> Test

testSendingMsgToClosedConnection protocol serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $
            L.serving protocol serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn <- L.open protocol serverAddr $ pure ()
            L.close conn
            res  <- L.send conn Success
            when (Left D.ConnectionClosed == res) $
                void $ L.notify succAddr Success

testSendingBigMsgByConnect :: (L.Send
                (D.Connection con) (Free L.NetworkingF),
              L.Connection (Free L.NodeF) con, Applicative f,
              L.Serving con (f ())) =>
             con -> D.PortNumber -> D.PortNumber -> Test
testSendingBigMsgByConnect protocol serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $
            L.serving protocol serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn <- L.open protocol serverAddr $ pure ()
            res  <- L.send conn bigMsg
            when (Left D.TooBigMessage == res) $
                void $ L.notify succAddr Success

testSendingBigUdpMsgByAddress :: D.PortNumber -> D.PortNumber -> Test
testSendingBigUdpMsgByAddress serverPort succPort =
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $
            L.serving D.Tcp serverPort $ pure ()
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            res <- L.notify serverAddr bigMsg
            when (Left D.TooBigMessage == res) $
                void $ L.notify succAddr Success

pingPongTest :: (L.Send (D.Connection con1) (Free L.NetworkingF),
                L.Send (D.Connection con2) m, Typeable con1, Typeable con2,
                Typeable m, L.Connection (Free L.NodeF) con1, L.Connection m con2,
                L.SendUdp m, Monad m,
                L.Serving con1 (Free (L.NetworkHandlerF con2 m) ())) =>
               con1 -> D.PortNumber -> D.PortNumber -> Test

pingPongTest protocol serverPort succPort = 
    runServingScenarion serverPort succPort $ \serverAddr succAddr nodeRt1 nodeRt2 -> do
        runNodeDefinitionL nodeRt1 $
            L.serving protocol serverPort $ do
                L.handler (pingHandle succAddr)
                L.handler (pongHandle succAddr)
        
        threadDelay 5000
        runNodeDefinitionL nodeRt2 $ do
            conn <- L.open protocol serverAddr $ do
                L.handler (pingHandle succAddr)
                L.handler (pongHandle succAddr)
            void $ L.send conn $ Ping 0

runServingScenarion
    :: D.PortNumber -> D.PortNumber -> (D.Address -> D.Address -> Rt.NodeRuntime -> Rt.NodeRuntime -> IO ()) -> Test
runServingScenarion serverPort succPort f = TestCase $ do
    let serverAddr = D.Address "127.0.0.1" serverPort
        succAddr    = D.Address "127.0.0.1" succPort
    nodeRt1 <- createNodeRuntime
    nodeRt2 <- createNodeRuntime
    void $ forkIO $ f serverAddr succAddr nodeRt1 nodeRt2
    ok <- succesServer succPort
    runNodeDefinitionL nodeRt1 $ L.stopServing serverPort
    assertBool "" ok    

pingHandle
    :: (L.Connection m con, L.SendUdp m, L.Send (D.Connection con) m, Monad m)
    => D.Address -> Ping -> D.Connection con -> m ()
pingHandle succAddr (Ping i) conn = do
    when (i < 10) $ void $ L.send conn (Pong $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAddr Success
        L.close conn

pongHandle
    :: (L.Connection m con, L.SendUdp m, L.Send (D.Connection con) m, Monad m)
    => D.Address -> Pong -> D.Connection con -> m ()
pongHandle succAddr (Pong i) conn = do
    when (i < 10) $ void $ L.send conn (Ping $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAddr Success
        L.close conn

emptFunc :: D.Connection D.Udp -> D.ConnectionVar D.Udp -> IO ()
emptFunc _ _ = pure ()

succesServer :: D.PortNumber -> IO Bool
succesServer port = do
    mvar <- newEmptyMVar
    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar False
    ch <- Con.startServer port (M.singleton (D.toTag Success) (\_ _ -> putMVar mvar True)) emptFunc putTextLn
    ok <- takeMVar mvar
    Enecuum.Prelude.atomically $ Con.stopServer ch
    pure ok
