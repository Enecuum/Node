{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass        #-}
module Enecuum.Tests.Integration.TcpServerSpec where

--

import           Enecuum.Prelude

import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )
import qualified Enecuum.Language              as L

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Interpreters
import           Enecuum.Language
import qualified Enecuum.Runtime as Rt
import qualified Enecuum.Domain                as D
import           Enecuum.Framework.Networking.Internal.Tcp.Connection
import qualified Data.Map as M
import qualified Enecuum.Framework.Networking.Internal.Connection     as Con


-- Tests disabled
spec :: Spec
spec = describe "TcpServer" $ fromHUnitTest $ TestList [TestLabel "Ping pong" pingPong]

createNodeRuntime :: IO Rt.NodeRuntime
createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= Rt.createNodeRuntime

newtype Ping = Ping Int deriving (Generic, ToJSON, FromJSON)
newtype Pong = Pong Int deriving (Generic, ToJSON, FromJSON)
data Succes = Succes    deriving (Generic, ToJSON, FromJSON)


pingHandle :: D.Connection D.Tcp -> Ping -> D.Connection D.Tcp -> L.NodeL ()
pingHandle succConn (Ping i) conn = do
    when (i < 10) $ L.send conn (Pong $ i + 1)
    when (i == 10) $ do
        L.send succConn Succes
        L.close conn

pongHandle :: D.Connection D.Tcp -> Pong -> D.Connection D.Tcp -> L.NodeL ()
pongHandle succConn (Pong i) conn = do
    when (i < 10) $ L.send conn (Ping $ i + 1)
    when (i == 10) $ do
        L.send succConn Succes
        L.close conn

pingPong :: Test
pingPong = TestCase $ do
    nr1 <- createNodeRuntime
    nr2 <- createNodeRuntime
    void $ forkIO $ do
        threadDelay 5000
        runNodeDefinitionL nr1 $ do
            succConn <- L.open D.Tcp succAdr $ pure ()
            L.serving D.Tcp serverPort $ do
                L.handler (pingHandle succConn)
                L.handler (pongHandle succConn)
        threadDelay 5000
        runNodeDefinitionL nr2 $ do
            succConn <- L.open D.Tcp succAdr $ pure ()
            conn     <- L.open D.Tcp serverAddr $ do
                L.handler (pingHandle succConn)
                L.handler (pongHandle succConn)
            L.send conn $ Ping 0
    ok <- succesServer succPort
    runNodeDefinitionL nr1 $ L.stopServing serverPort
    assertBool "" ok

emptFunc :: D.Connection D.Tcp -> D.ConnectionVar D.Tcp -> IO ()
emptFunc _ _ = pure ()

succesServer :: PortNumber -> IO Bool
succesServer port = do
    mvar <- newEmptyMVar
    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar False
    ch <- Con.startServer port (M.singleton (D.toTag Succes) (\_ _ -> putMVar mvar True)) emptFunc
    ok <- takeMVar mvar
    Enecuum.Prelude.atomically $ Con.stopServer ch
    pure ok

serverPort, succPort :: PortNumber
serverPort = 2000
succPort = 3000

serverAddr, succAdr :: D.Address
serverAddr = D.Address "127.0.0.1" serverPort
succAdr = D.Address "127.0.0.1" succPort
