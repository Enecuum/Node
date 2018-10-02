{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving#-}

module Enecuum.Tests.Integration.TcpServerSpec where

--

import           Enecuum.Prelude

import           Data.Aeson as A
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )
import qualified Enecuum.Language              as L

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Interpreters
import           Enecuum.Language
import qualified Enecuum.Framework.Domain.RPC as R
import           Enecuum.Framework.Domain.RPC
import qualified Enecuum.Runtime as Rt
import           Enecuum.Framework.Node.Language          ( NodeL )
import qualified Enecuum.Domain                as D
import           Enecuum.Framework.Networking.Interpreter
import           Enecuum.Framework.MsgHandler.Language
import           Enecuum.Framework.Networking.Internal
import qualified Data.Map as M



-- Tests disabled
spec :: Spec
spec = describe "TcpServer" $ fromHUnitTest $ TestList
    [ TestLabel "Ping pong" pingPong]

createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= Rt.createNodeRuntime

data Ping = Ping Int
data Pong = Pong Int
data Succes = Succes

instance A.ToJSON Succes where
    toJSON o = A.object
        [ "tag" A..= makeTagName o
        ]

instance A.FromJSON Succes where
    parseJSON _ = pure $ Succes

instance A.ToJSON Ping where
    toJSON o@(Ping i) = A.object
        [ "tag" A..= makeTagName o
        , "int" A..= i
        ]

instance A.ToJSON Pong where
    toJSON o@(Pong i) = A.object
        [ "tag" A..= makeTagName o
        , "int" A..= i
        ]

instance A.FromJSON Ping where
    parseJSON (A.Object o) = Ping <$> o A..: "int"

instance A.FromJSON Pong where
    parseJSON (A.Object o) = Pong <$> o A..: "int"


pingHandle :: D.NetworkConnection -> Ping -> D.NetworkConnection -> L.NodeL ()
pingHandle succ (Ping i) conn = do
    when (i < 10)  $ L.send conn (Pong $ i+1)
    when (i == 10) $ do
        L.send succ Succes
        L.close conn

pongHandle :: D.NetworkConnection -> Pong -> D.NetworkConnection -> L.NodeL ()
pongHandle succ (Pong i) conn = do
    when (i < 10)  $ L.send conn (Ping $ i+1)
    when (i == 10) $ do
        L.send succ Succes
        L.close conn

pingPong :: Test
pingPong = TestCase $ do
    nr1 <- createNodeRuntime
    nr2 <- createNodeRuntime
    void $ forkIO $ do
        threadDelay 10000
        runNodeDefinitionL nr1 $ do
            succ <- L.open succAdr $ return ()
            L.serving serverPort $ do
                L.handler (pingHandle succ)
                L.handler (pongHandle succ)
        threadDelay 10000
        runNodeDefinitionL nr2 $ do
            succ <- L.open succAdr $ return ()
            conn <- L.open serverAddr $ do
                L.handler (pingHandle succ)
                L.handler (pongHandle succ)
            L.send conn $ Ping 0
    ok <- succesServer succPort
    runNodeDefinitionL nr1 $ L.stopServing serverPort
    assertBool "" ok

succesServer :: PortNumber -> IO Bool
succesServer port = do
    mvar <- newEmptyMVar
    void $ forkIO $ do
        threadDelay 1000000
        putMVar mvar False
    ch <- startServer port $ M.singleton
        (makeTagName Succes)
        (\_ _ -> putMVar mvar True)
    ok <- takeMVar mvar
    Enecuum.Prelude.atomically $ stopServer ch
    return ok

serverPort = 2000
succPort   = 3000
serverAddr = D.Address "127.0.0.1" serverPort
succAdr    = D.Address "127.0.0.1" succPort