{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Client where

import           Enecuum.Prelude

import           Enecuum.Config
import qualified Enecuum.Domain as D
import qualified Data.Aeson as A
import qualified Enecuum.Language as L
import qualified Enecuum.Assets.Nodes.Messages as M
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Framework.Language.Extra (HasGraph, HasStatus, NodeStatus (..))

data GetLastKBlock               = GetLastKBlock  D.Address
data GetWalletBalance            = GetWalletBalance M.WalletId D.Address
data StartForeverChainGeneration = StartForeverChainGeneration
data StartNBlockPacketGeneration = StartNBlockPacketGeneration {number :: Int}
data Ping                        = Ping (D.Protocol Int) D.Address

data GraphNodeData = GraphNodeData
    { _blockchain    :: D.BlockchainData
    , _logVar        :: D.StateVar [Text]
    , _status        :: D.StateVar NodeStatus
    }

instance A.FromJSON Ping where
    parseJSON = A.withObject "Ping" $ \o -> Ping <$> (o A..: "protocol") <*> (o A..: "address")

instance A.FromJSON GetWalletBalance where
    parseJSON = A.withObject "GetWalletBalance" $ \o -> GetWalletBalance <$> o A..: "walletID" <*> (o A..: "address")

instance A.FromJSON GetLastKBlock where
    parseJSON = A.withObject "GetLastKBlock" $ \o -> GetLastKBlock <$> (o A..: "address")

instance A.FromJSON StartForeverChainGeneration where
    parseJSON _ = pure StartForeverChainGeneration

instance A.FromJSON StartNBlockPacketGeneration where
    parseJSON = A.withObject "StartNBlockPacketGeneration" $ \o -> StartNBlockPacketGeneration <$> o A..: "number"

--sendPing = 

sendRequestToPoW :: forall a. (ToJSON a, Typeable a) => a -> L.NodeL Text
sendRequestToPoW request = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest powNodeRpcAddress request
    pure . eitherToText $ res

startForeverChainGenerationHandler :: StartForeverChainGeneration -> L.NodeL Text
startForeverChainGenerationHandler _ = sendRequestToPoW M.ForeverChainGeneration

startNBlockPacketGenerationHandler :: StartNBlockPacketGeneration -> L.NodeL Text
startNBlockPacketGenerationHandler (StartNBlockPacketGeneration i) = sendRequestToPoW $ M.NBlockPacketGeneration i

getLastKBlockHandler :: GetLastKBlock -> L.NodeL Text
getLastKBlockHandler (GetLastKBlock address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure . eitherToText $ res

getWalletBalance :: GetWalletBalance -> L.NodeL Text
getWalletBalance (GetWalletBalance walletId address) = do
    res :: Either Text M.WalletBalanceMsg <- L.makeRpcRequest address (M.GetWalletBalance walletId)
    pure . eitherToText $ res


ping :: Ping -> L.NodeL Text
ping (Ping D.TCP address) = do
    res <- L.withConnection D.Tcp address $ \conn -> L.send conn M.Ping
    pure $ case res of Right _ -> "Tcp port is available."; Left _ -> "Tcp port is not available."

ping (Ping D.RPC address) = do
    res :: Either Text M.Pong <- L.makeRpcRequest address M.Ping
    pure $ case res of Right _ -> "Tcp port is available."; Left _ -> "Tcp port is not available."

ping (Ping D.UDP address) = pure $ "This functionality is not supported."

{-
Requests:
{"method":"GetLastKBlock", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"StartForeverChainGeneration"}
{"method":"StartNBlockPacketGeneration", "number" : 2}
{"method":"StartNBlockPacketGeneration", "number" : 1}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2009}}
{"method":"StopNode"}
{"method":"Ping", "protocol":"RPC", "address":{"host":"127.0.0.1", "port": 2008}}
-}

clientNode :: L.NodeDefinitionL ()
clientNode = do
    L.logInfo "Client started"
    L.nodeTag "Client"
    stateVar <- L.scenario $ L.atomically $ L.newVar NodeActing
    L.std $ do
        L.stdHandler $ getLastKBlockHandler
        L.stdHandler $ getWalletBalance
        L.stdHandler startForeverChainGenerationHandler
        L.stdHandler startNBlockPacketGenerationHandler
        L.stdHandler $ L.stopNodeHandler' stateVar
        L.stdHandler ping
    L.awaitNodeFinished' stateVar

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a
