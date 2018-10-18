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

data GetLastKBlock               = GetLastKBlock
newtype GetWalletBalance         = GetWalletBalance M.WalletId
data StartForeverChainGeneration = StartForeverChainGeneration
data StartNBlockPacketGeneration = StartNBlockPacketGeneration {number :: Int}
data Ping = Ping (D.Protocol Int) D.Address

instance A.FromJSON Ping where
    parseJSON = A.withObject "Ping" $ \o -> Ping <$> (o A..: "protocol") <*> (o A..: "address")

instance A.FromJSON GetWalletBalance where
    parseJSON = A.withObject "GetWalletBalance" $ \o -> GetWalletBalance <$> o A..: "walletID"

instance A.FromJSON GetLastKBlock where
    parseJSON _ = pure GetLastKBlock

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

getLastKBlockHandler :: D.Address -> GetLastKBlock -> L.NodeL Text
getLastKBlockHandler address _ = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure . eitherToText $ res

getWalletBalance :: D.Address -> GetWalletBalance -> L.NodeL Text
getWalletBalance address (GetWalletBalance walletId) = do
    res :: Either Text M.WalletBalanceMsg <- L.makeRpcRequest address (M.GetWalletBalance walletId)
    pure . eitherToText $ res


ping :: Ping -> L.NodeL Text
ping (Ping D.TCP address) = do
    res <- L.withConnection D.Tcp address $ \conn -> L.send conn M.Ping
    pure $ case res of Right _ -> "Tcp port is available."; Left _ -> "Tcp port not available."

ping (Ping D.RPC address) = do
    res :: Either Text M.Pong <- L.makeRpcRequest powNodeRpcAddress M.Ping
    pure $ case res of Right _ -> "Rpc port is available."; Left _ -> "Rpc port not available."

ping (Ping D.UDP address) = pure $ "This functionality is not supported."

{-
Requests:
    {"method":"GetLastKBlock"}
    {"method":"StartForeverChainGeneration"}
    {"method":"StartNBlockPacketGeneration", "number" : 2}
    {"method":"StartNBlockPacketGeneration", "number" : 1}
    {"method":"GetWalletBalance", "walletID": 2}
    {"method":"StopNode"}
    {"method":"Ping", "protocol":"RPC", "address":{"host":"127.0.0.1", "port": 2001}}
-}

clientNode :: Config -> L.NodeDefinitionL ()
clientNode config = do
    L.logInfo "Client started"
    L.nodeTag "Client"
    case readClientConfig config of
        Nothing -> pure ()
        Just address -> do
            
            stateVar <- L.scenario $ L.atomically $ L.newVar NodeActing

            L.std $ do
                L.stdHandler $ getLastKBlockHandler address
                L.stdHandler $ getWalletBalance address
                L.stdHandler startForeverChainGenerationHandler
                L.stdHandler startNBlockPacketGenerationHandler
                L.stdHandler $ L.stopNodeHandler' stateVar
                L.stdHandler ping

            L.awaitNodeFinished' stateVar

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a
