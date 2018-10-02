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

-- Tests disabled
spec :: Spec
spec = describe "TcpServer" $ fromHUnitTest $ TestList
    [ TestLabel "Ping pong" pingPong]

createNodeRuntime = Rt.createVoidLoggerRuntime >>= Rt.createCoreRuntime >>= Rt.createNodeRuntime

data Ping = Ping Int
data Pong = Pong Int

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


pingHandle :: Ping -> D.NetworkConnection -> L.NodeL ()
pingHandle (Ping i) conn = do
    L.logInfo $ "Ping:" +|| i ||+ "."
    when (i < 10)  $ L.send conn (Pong $ i+1)
    when (i == 10) $ L.close conn

pongHandle :: Pong -> D.NetworkConnection -> L.NodeL ()
pongHandle (Pong i) conn = do
    L.logInfo $ "Pong:" +|| i ||+ "."
    when (i < 10)  $ L.send conn (Ping $ i+1)
    when (i == 10) $ L.close conn

pingPong :: Test
pingPong = TestCase $ do
    nr1 <- createNodeRuntime
    nr2 <- createNodeRuntime
    runNodeDefinitionL nr1 $ L.serving serverPort $ do
        L.handler pingHandle
        L.handler pongHandle
    
    runNodeDefinitionL nr2 $ do
        conn <- L.open serverAddr $ do
            L.handler pingHandle
            L.handler pongHandle
        L.send conn $ Ping 0
    

serverPort = 2000
serverAddr = D.Address "127.0.0.1" serverPort
