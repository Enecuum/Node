{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Client where

import           Enecuum.Prelude

import qualified Enecuum.Domain as D
import qualified Data.Aeson as A
import qualified Enecuum.Language as L
import qualified Enecuum.Assets.Nodes.Messages as M
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Framework.Language.Extra (HasFinished)

data GetLastKBlock       = GetLastKBlock
newtype GetWalletBalance = GetWalletBalance M.WalletId
data StartForeverChainGeneration = StartForeverChainGeneration
data StartNBlockPacketGeneration = StartNBlockPacketGeneration {number :: Int}

instance A.FromJSON GetWalletBalance where
    parseJSON = A.withObject "GetWalletBalance" $ \o -> GetWalletBalance <$> o A..: "walletID"

instance A.FromJSON GetLastKBlock where
    parseJSON _ = pure GetLastKBlock

instance A.FromJSON StartForeverChainGeneration where
    parseJSON _ = pure StartForeverChainGeneration

instance A.FromJSON StartNBlockPacketGeneration where
    parseJSON = A.withObject "StartNBlockPacketGeneration" $ \o -> StartNBlockPacketGeneration <$> o A..: "number"


data ClientNodeData = ClientNodeData
    { _finished :: D.StateVar Bool
    }

makeFieldsNoPrefix ''ClientNodeData

sendRequestToPoW request = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest powNodeRpcAddress request
    pure . eitherToText $ res

startForeverChainGenerationHandler :: StartForeverChainGeneration -> L.NodeL Text
startForeverChainGenerationHandler _ = sendRequestToPoW M.ForeverChainGeneration

startNBlockPacketGenerationHandler :: StartNBlockPacketGeneration -> L.NodeL Text
startNBlockPacketGenerationHandler (StartNBlockPacketGeneration i) = sendRequestToPoW $ M.NBlockPacketGeneration i

getLastKBlockHandler :: GetLastKBlock -> L.NodeL Text
getLastKBlockHandler _ = do
    res :: Either Text D.KBlock <- L.makeRpcRequest graphNodeRpcAddress M.GetLastKBlock
    pure . eitherToText $ res

getWalletBalance :: GetWalletBalance -> L.NodeL Text
getWalletBalance (GetWalletBalance walletId) = do
    res :: Either Text M.WalletBalanceMsg <- L.makeRpcRequest graphNodeRpcAddress (M.GetWalletBalance walletId)
    pure . eitherToText $ res

{-
Requests:
    {"method":"GetLastKBlock"}
    {"method":"StartForeverChainGeneration"}
    {"method":"StartNBlockPacketGeneration", "number" : 2}
    {"method":"StartNBlockPacketGeneration", "number" : 1}
    {"method":"GetWalletBalance", "walletID": 2}
    {"method":"NodeFinished"}
-}

clientNode :: L.NodeDefinitionL ()
clientNode = do
    L.logInfo "Client started"
    nodeData <- ClientNodeData <$> L.scenario (L.atomically $ L.newVar True)
    L.std $ do
        L.stdHandler getLastKBlockHandler
        L.stdHandler getWalletBalance
        L.stdHandler startForeverChainGenerationHandler
        L.stdHandler startNBlockPacketGenerationHandler
        L.stdHandler $ L.setNodeFinished nodeData

    L.nodeFinishPending nodeData

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a
