{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.Client where

import qualified Data.Aeson                       as A
import qualified Data.Map                         as Map
import           Data.Text                        hiding (map)
import           Enecuum.Assets.Nodes.Address
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.System.Directory  as D (keysFilePath)
import qualified Enecuum.Blockchain.Lens          as Lens
import           Enecuum.Config
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasGraph, HasStatus, NodeStatus (..))
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude                  hiding (map, unpack)

data AcceptTransaction           = AcceptTransaction D.CLITransaction D.Address deriving ( Generic, Show, Eq, Ord, ToJSON)
data GetLastKBlock               = GetLastKBlock               D.Address
data GetWalletBalance            = GetWalletBalance M.WalletId D.Address
data GetLengthOfChain            = GetLengthOfChain            D.Address
data StartForeverChainGeneration = StartForeverChainGeneration D.Address
data StartNBlockPacketGeneration = StartNBlockPacketGeneration Int D.Address
data Ping                        = Ping (D.Protocol Int) D.Address
data StopRequest                 = StopRequest                 D.Address
data GetBlock                    = GetBlock       D.StringHash D.Address

data GraphNodeData = GraphNodeData
    { _blockchain :: D.BlockchainData
    , _logVar     :: D.StateVar [Text]
    , _status     :: D.StateVar NodeStatus
    }

instance A.FromJSON Ping where
    parseJSON = A.withObject "Ping" $ \o -> Ping <$> (o A..: "protocol") <*> (o A..: "address")

instance A.FromJSON GetWalletBalance where
    parseJSON = A.withObject "GetWalletBalance" $ \o -> GetWalletBalance <$> o A..: "walletID" <*> (o A..: "address")

instance A.FromJSON AcceptTransaction where
    parseJSON = A.withObject "AcceptTransaction" $ \o -> AcceptTransaction <$> o A..: "tx" <*> (o A..: "address")

instance A.FromJSON GetLastKBlock where
    parseJSON = A.withObject "GetLastKBlock" $ \o -> GetLastKBlock <$> (o A..: "address")

instance A.FromJSON GetLengthOfChain where
    parseJSON = A.withObject "GetLengthOfChain" $ \o -> GetLengthOfChain <$> (o A..: "address")

instance A.FromJSON StartForeverChainGeneration where
    parseJSON = A.withObject "StartForeverChainGeneration" $ \o -> StartForeverChainGeneration <$> (o A..: "address")

instance A.FromJSON StartNBlockPacketGeneration where
    parseJSON = A.withObject "StartNBlockPacketGeneration" $ \o -> StartNBlockPacketGeneration <$> o A..: "number" <*> (o A..: "address")

instance A.FromJSON StopRequest where
    parseJSON = A.withObject "StopRequest" $ \o -> StopRequest <$> (o A..: "address")

instance A.FromJSON GetBlock where
    parseJSON = A.withObject "GetBlock" $ \o -> GetBlock <$> o A..: "hash" <*> (o A..: "address")

sendSuccessRequest :: forall a. (ToJSON a, Typeable a) => D.Address -> a -> L.NodeL Text
sendSuccessRequest address request = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address request
    pure . eitherToText $ res

startForeverChainGenerationHandler :: StartForeverChainGeneration -> L.NodeL Text
startForeverChainGenerationHandler (StartForeverChainGeneration address) = sendSuccessRequest address M.ForeverChainGeneration

startNBlockPacketGenerationHandler :: StartNBlockPacketGeneration -> L.NodeL Text
startNBlockPacketGenerationHandler (StartNBlockPacketGeneration i address) = sendSuccessRequest address $ M.NBlockPacketGeneration i

getLastKBlockHandler :: GetLastKBlock -> L.NodeL Text
getLastKBlockHandler (GetLastKBlock address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure . eitherToText $ res

getWalletBalance :: GetWalletBalance -> L.NodeL Text
getWalletBalance (GetWalletBalance walletId address) = do
    res :: Either Text M.WalletBalanceMsg <- L.makeRpcRequest address (M.GetWalletBalance walletId)
    pure . eitherToText $ res

transform :: (L.ERandom m, L.Logger m, L.FileSystem m, Monad m) => D.CLITransaction -> m D.Transaction
transform tx = do
    -- L.logInfo $ show tx
    keys <- lines <$> (L.readFile =<< keysFilePath)
    let wallets = fmap ( D.transformWallet . (\a -> read a :: D.CLIWallet0) . unpack) keys
    -- L.logInfo $ show $ wallets   
    let walletsMap = Map.fromList $ fmap (\w -> (w ^. Lens.name, w))  wallets
    let ownerName  = tx ^. Lens.owner
        receiverName =  tx ^. Lens.receiver
        ownerM = Map.lookup ownerName walletsMap
        receiverM = Map.lookup receiverName walletsMap
    let (ownerPub, ownerPriv) = case ownerM of
                    Nothing -> error $ "There is no record for owner: " +|| show ownerName
                    Just j -> (pub, priv)
                                where pub = j ^. Lens.publicKey
                                      priv = fromJust $ j ^. Lens.privateKey
        receiverPub = case receiverM of
                        Nothing -> read receiverName :: D.CLIPublicKey
                        Just j -> j ^. Lens.publicKey            
    let (D.CLIPublicKey owner1) = ownerPub 
        (D.CLIPrivateKey ownerPriv1) = ownerPriv
        (D.CLIPublicKey receiver1) = receiverPub
        amount1 = tx ^. Lens.amount 
        currency1 = tx ^. Lens.currency
    txSigned <- D.signTransaction owner1 ownerPriv1 receiver1 amount1 currency1
    L.logInfo $ "Client send transaction " +|| show txSigned    
    pure txSigned
    -- undefined

createTransaction :: AcceptTransaction -> L.NodeL Text
createTransaction (AcceptTransaction tx address) = do
    transaction <- transform tx
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address (M.AcceptTransaction transaction)
    pure . eitherToText $ res

getLengthOfChain :: GetLengthOfChain -> L.NodeL Text
getLengthOfChain (GetLengthOfChain address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    case res of
        Right kBlock -> pure $ "Length of chain is " <> show (D._number kBlock)
        Left t       -> pure t

ping :: Ping -> L.NodeL Text
ping (Ping D.TCP address) = do
    res <- L.withConnection D.Tcp address $ \conn -> L.send conn M.Ping
    pure $ case res of Right _ -> "Tcp port is available."; Left _ -> "Tcp port is not available."

ping (Ping D.RPC address) = do
    res :: Either Text M.Pong <- L.makeRpcRequest address M.Ping
    pure $ case res of Right _ -> "Tcp port is available."; Left _ -> "Tcp port is not available."

ping (Ping D.UDP _) = pure $ "This functionality is not supported."

stopRequest :: StopRequest -> L.NodeL Text
stopRequest (StopRequest address) = sendSuccessRequest address M.Stop

getBlock :: GetBlock -> L.NodeL Text
getBlock (GetBlock hash address) = do
    res :: Either Text D.NodeContent <- L.makeRpcRequest address (M.GetGraphNode hash)
    case res of
        Right (D.KBlockContent block) -> pure $ "Key block is "   <> show block
        Right (D.MBlockContent block) -> pure $ "Microblock is " <> show block
        Left  text                    -> pure $ "Error: "         <> text

    {-
Requests:
{"method":"GetLastKBlock", "address":{"host":"127.0.0.1", "port": 2005}}
{"method":"StartForeverChainGeneration", "address":{"host":"127.0.0.1", "port": 2005}}
{"method":"StartNBlockPacketGeneration", "number" : 2, "address":{"host":"127.0.0.1", "port": 2005}}
{"method":"StartNBlockPacketGeneration", "number" : 1, "address":{"host":"127.0.0.1", "port": 2005}}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2009}}
{"method":"StopNode"}
{"method":"Ping", "protocol":"RPC", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"GetLengthOfChain", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"StopRequest", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"AcceptTransaction", "tx": {"amount":15, "owner": "me", "receiver":"Alice","currency": "ENQ"}, "address":{"host":"127.0.0.1", "port": 2008}}
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
        L.stdHandler getLengthOfChain
        L.stdHandler ping
        L.stdHandler stopRequest
        L.stdHandler getBlock
        L.stdHandler $ createTransaction
    L.awaitNodeFinished' stateVar

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a
