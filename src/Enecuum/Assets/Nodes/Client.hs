module Enecuum.Assets.Nodes.Client where

import           Enecuum.Prelude

import qualified Enecuum.Domain as D
import qualified Data.Aeson as A
import qualified Enecuum.Language as L
import qualified Enecuum.Assets.Nodes.Messages as M
import           Enecuum.Assets.Nodes.Address

data GetLastKBlock       = GetLastKBlock
newtype GetWalletBalance = GetWalletBalance M.WalletId
data StartForeverChainGeneration = StartForeverChainGeneration
data StartNBlockPacketGeneration = StartNBlockPacketGeneration {number :: Int}

instance A.FromJSON GetWalletBalance where
    parseJSON (A.Object o) = GetWalletBalance <$> o A..: "walletID"

instance A.FromJSON GetLastKBlock where
    parseJSON _ = pure GetLastKBlock

instance A.FromJSON StartForeverChainGeneration where
    parseJSON _ = pure StartForeverChainGeneration

instance A.FromJSON StartNBlockPacketGeneration where
    parseJSON (A.Object o) = StartNBlockPacketGeneration <$> o A..: "number"

sendRequestToPoW request = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest powAddr request
    pure . eitherToText $ res

startForeverChainGenerationHandler :: StartForeverChainGeneration -> L.NodeL Text
startForeverChainGenerationHandler _ = sendRequestToPoW M.ForeverChainGeneration

startNBlockPacketGenerationHandler :: StartNBlockPacketGeneration -> L.NodeL Text
startNBlockPacketGenerationHandler (StartNBlockPacketGeneration i) = 
    sendRequestToPoW $ M.NBlockPacketGeneration i

getLastKBlockHandler :: GetLastKBlock -> L.NodeL Text
getLastKBlockHandler _ = do
    res :: Either Text D.KBlock <-
        L.makeRpcRequest graphNodeRpcAddress M.GetLastKBlock
    pure . eitherToText $ res

getWalletBalance :: GetWalletBalance -> L.NodeL Text
getWalletBalance (GetWalletBalance walletId) = do
    res :: Either Text M.WalletBalanceMsg <-
        L.makeRpcRequest graphNodeRpcAddress (M.GetWalletBalance walletId)
    pure . eitherToText $ res

{-
Requests:
    {"method":"GetLastKBlock"}
    {"method":"StartForeverChainGeneration"}
    {"method":"StartNBlockPacketGeneration", "number" : 2}
    {"method":"GetWalletBalance", "walletID": 3}
-}

clientNode :: L.NodeDefinitionL ()
clientNode = do
    L.logInfo "Client started"
    L.std $ do
        L.stdHandler getLastKBlockHandler
        L.stdHandler getWalletBalance
        L.stdHandler startForeverChainGenerationHandler
        L.stdHandler startNBlockPacketGenerationHandler

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left a)  = "Server error: " <> a
eitherToText (Right a) = show a
