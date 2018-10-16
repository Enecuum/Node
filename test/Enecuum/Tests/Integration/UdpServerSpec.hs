{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass        #-}
module Enecuum.Tests.Integration.UdpServerSpec where

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



-- Tests disabled
spec :: Spec
spec = describe "UdpServer" $ fromHUnitTest $ TestList
    [ TestLabel "Ping pong"          pingPong
    , TestLabel "client server test" clientServerTest
    , TestLabel "3 - nodes test"     threeNodesTest
    ]

createNodeRuntime :: IO Rt.NodeRuntime
createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= Rt.createNodeRuntime

newtype Ping = Ping Int deriving (Generic, ToJSON, FromJSON)
newtype Pong = Pong Int deriving (Generic, ToJSON, FromJSON)
data Succes = Succes    deriving (Generic, ToJSON, FromJSON)


pingHandle :: Ping -> D.Connection D.Udp -> L.NodeL ()
pingHandle (Ping i) conn = do
    when (i < 10) $ void $ L.send conn (Pong $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAdr Succes
        L.close conn

pongHandle :: Pong -> D.Connection D.Udp -> L.NodeL ()
pongHandle (Pong i) conn = do
    when (i < 10) $ void $ L.send conn (Ping $ i + 1)
    when (i == 10) $ do
        void $ L.notify succAdr Succes
        L.close conn

pingPong :: Test
pingPong = TestCase $ do
    nr1 <- createNodeRuntime
    nr2 <- createNodeRuntime
    void $ forkIO $ do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            L.serving D.Udp serverPort $ do
                L.handler pingHandle
                L.handler pongHandle
        threadDelay 5000
        runNodeDefinitionL nr2 $ do
            conn <- L.open D.Udp serverAddr $ do
                L.handler pingHandle
                L.handler pongHandle
            void $ L.send conn $ Ping 0
    ok <- succesServer succPort
    runNodeDefinitionL nr1 $ L.stopServing serverPort
    assertBool "" ok

emptFunc :: D.Connection D.Udp -> D.ConnectionVar D.Udp -> IO ()
emptFunc _ _ = pure ()

succesServer :: PortNumber -> IO Bool
succesServer port = do
    mvar <- newEmptyMVar
    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar False
    ch <- startServer port (M.singleton (D.toTag Succes) (\_ _ -> putMVar mvar True)) emptFunc putTextLn
    ok <- takeMVar mvar
    Enecuum.Prelude.atomically $ stopServer ch
    pure ok

serverPort, succPort :: PortNumber
serverPort = 4000
succPort = 5000

serverAddr, succAdr :: D.Address
serverAddr = D.Address "127.0.0.1" serverPort
succAdr = D.Address "127.0.0.1" succPort

clientServerTest = TestCase $ do
    mvar <- newEmptyMVar
    chan <- atomically $ newTChan 
    void $ forkIO $ runUDPServer chan 7000 (\_ _ _ -> putMVar mvar True)
    threadDelay 10000
    void $ forkIO $ runClient D.UDP (D.Address "127.0.0.1" 7000) $
        \sock -> void $ S.send sock "ok" 
    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar False
    ok <- takeMVar mvar
    assertBool "" ok

threeNodesTest = TestCase $ do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar
    chan <- atomically $ newTChan 
    void $ forkIO $ runUDPServer chan 8000 (\_ _ _ -> do
        whenM   (isEmptyMVar mvar1) $ putMVar mvar1 True
        unlessM (isEmptyMVar mvar1) $ putMVar mvar2 True
        )
    threadDelay 10000
    void $ forkIO $ runClient D.UDP (D.Address "127.0.0.1" 8000) $
        \sock -> void $ S.send sock "ok" 
    threadDelay 10000
    void $ forkIO $ runClient D.UDP (D.Address "127.0.0.1" 8000) $
        \sock -> void $ S.send sock "ok" 

    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar2 False
    
    ok <- takeMVar mvar2
    assertBool "" ok