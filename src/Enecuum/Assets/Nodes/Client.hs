{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.Client where

import qualified Data.Aeson                           as A
import           Data.Aeson.Extra                     (noLensPrefix)
import qualified Data.Map                             as Map
import           Data.Text                            hiding (map)
import           Enecuum.Assets.Blockchain.Generation as D
import           Enecuum.Assets.Nodes.Address
import qualified Enecuum.Assets.Nodes.Messages        as M
import           Enecuum.Assets.System.Directory      as D (keysFilePath, wrongKeysFilePath)
import qualified Enecuum.Blockchain.Lens              as Lens
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import           Enecuum.Framework.Language.Extra     (HasGraph, HasStatus, NodeStatus (..))
import qualified Enecuum.Language                     as L
import           Enecuum.Prelude                      hiding (map, unpack)

type BlocksCount = Int

data AcceptTransaction           = AcceptTransaction CLITransaction D.Address deriving ( Generic, Show, Eq, Ord, ToJSON)
data GetLastKBlock               = GetLastKBlock D.Address
data GetWalletBalance            = GetWalletBalance Int D.Address
data GetLengthOfChain            = GetLengthOfChain D.Address
data StartForeverChainGeneration = StartForeverChainGeneration D.Address
data GenerateBlocksPacket        = GenerateBlocksPacket BlocksCount D.Address
data Ping                        = Ping (D.Protocol Int) D.Address
data StopRequest                 = StopRequest D.Address
data GetBlock                    = GetBlock D.StringHash D.Address

data GraphNodeData = GraphNodeData
    { _blockchain :: D.BlockchainData
    , _logVar     :: D.StateVar [Text]
    , _status     :: D.StateVar NodeStatus
    }


data CLITransaction = CLITransaction
  { _owner    :: String
  , _receiver :: String
  , _amount   :: D.Amount
  , _currency :: D.Currency
  } deriving ( Generic, Show, Eq, Ord, Read, Serialize)

  
instance ToJSON CLITransaction where toJSON = genericToJSON noLensPrefix
instance FromJSON CLITransaction where parseJSON = genericParseJSON noLensPrefix


data CLIPublicKey  = CLIPublicKey  D.PublicKey  deriving ( Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, Serialize)
data CLIPrivateKey = CLIPrivateKey D.PrivateKey deriving ( Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, Serialize)


data CLIWallet0 = CLIWallet0
  { _id         :: Int
  , _name       :: String
  , _publicKey  :: String
  , _privateKey :: Maybe String 
  } deriving (Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

data CLIWallet = CLIWallet
  { _id         :: Int
  , _name       :: String
  , _publicKey  :: CLIPublicKey
  , _privateKey :: Maybe CLIPrivateKey
  } deriving (Generic, Show, Eq, Ord, Read, ToJSON, FromJSON)

transformWallet :: CLIWallet0 -> CLIWallet
transformWallet CLIWallet0 {..} = CLIWallet
  { _id         = _id
  , _name       = _name
  , _publicKey  = CLIPublicKey (D.readPublicKey _publicKey)
  , _privateKey = privKey
  }
  where privKey = case _privateKey of
                    Nothing -> Nothing
                    Just j -> Just $ CLIPrivateKey (D.readPrivateKey j) 

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

instance A.FromJSON GenerateBlocksPacket where
    parseJSON = A.withObject "GenerateBlocksPacket" $ \o -> GenerateBlocksPacket <$> o A..: "blocks" <*> (o A..: "address")

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

generateBlocksPacketHandler :: GenerateBlocksPacket -> L.NodeL Text
generateBlocksPacketHandler (GenerateBlocksPacket i address) = sendSuccessRequest address $ M.NBlockPacketGeneration i

getLastKBlockHandler :: GetLastKBlock -> L.NodeL Text
getLastKBlockHandler (GetLastKBlock address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure . eitherToText $ res

idToKey :: (L.FileSystem m, Monad m) => Int -> m (D.PublicKey, String)
idToKey n = do
    keys <- lines <$> (L.readFile =<< keysFilePath)
    let wallets = fmap (transformWallet . (\a -> read a :: CLIWallet0) . unpack) keys
    let walletsMap = Map.fromList $ fmap (\w -> (_id (w :: CLIWallet), w)) wallets
    case Map.lookup n walletsMap of
        Nothing -> error $ "There is no wallet with id: " +| n |+ ""
        Just j -> do
            let (CLIPublicKey pubKey) = _publicKey (j :: CLIWallet)
            pure (pubKey, _name (j :: CLIWallet))

getWalletBalance :: GetWalletBalance -> L.NodeL Text
getWalletBalance (GetWalletBalance walletId address) = do
    (pubKey, name) <- idToKey walletId
    res :: Either Text M.WalletBalanceMsg <- L.makeRpcRequest address (M.GetWalletBalance pubKey)
    let fun :: M.WalletBalanceMsg -> Text
        fun (M.WalletBalanceMsg _ balance) = "" +|| name ||+ " : " +|| balance ||+ ""
    pure $ eitherToText2 $ fmap fun res

transform :: (L.ERandom m, L.Logger m, L.FileSystem m, Monad m) => CLITransaction -> ScenarioRole -> m D.Transaction
transform tx _ = do
    filePath <- keysFilePath
    keys <- lines <$> (L.readFile filePath)
    let wallets = fmap (transformWallet . (\a -> read a :: CLIWallet0) . unpack) keys
    let walletsMap = Map.fromList $ fmap (\w -> (_name (w :: CLIWallet), w))  wallets
    let ownerName  = _owner tx
    let receiverName = _receiver tx
    let ownerM = Map.lookup ownerName walletsMap
    let receiverM = Map.lookup receiverName walletsMap
    let (ownerPub, ownerPriv) = case ownerM of
                    Nothing -> error $ "There is no record for owner: " +|| ownerName ||+ "."
                    Just  j -> (pub, priv)
                                where pub = _publicKey (j :: CLIWallet)
                                      priv = fromJust $ _privateKey (j :: CLIWallet)
        receiverPub = case receiverM of
                        Nothing -> read receiverName :: CLIPublicKey
                        Just j  -> _publicKey (j :: CLIWallet)
    let (CLIPublicKey owner1) = ownerPub
        (CLIPrivateKey ownerPriv1) = ownerPriv
        (CLIPublicKey receiver1) = receiverPub
        amount1 = _amount tx
        currency1 = _currency tx
    txSigned <- D.signTransaction owner1 ownerPriv1 receiver1 amount1 currency1
    pure txSigned

createTransaction :: ScenarioRole -> AcceptTransaction -> L.NodeL Text
createTransaction role  (AcceptTransaction tx address) = do
    transaction <- transform tx role
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
{"method":"GenerateBlocksPacket", "blocks" : 2, "address":{"host":"127.0.0.1", "port": 2005}}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2009}}
{"method":"StopNode"}
{"method":"Ping", "protocol":"RPC", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"GetLengthOfChain", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"StopRequest", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"AcceptTransaction", "tx": {"amount":15, "owner": "me", "receiver":"Alice","currency": "ENQ"}, "address":{"host":"127.0.0.1", "port": 2008}}
-}

clientNode :: ScenarioRole -> L.NodeDefinitionL ()
clientNode role = do
    L.logInfo "Client started"
    L.nodeTag "Client"
    stateVar <- L.scenario $ L.atomically $ L.newVar NodeActing
    L.std $ do
        L.stdHandler $ getLastKBlockHandler
        L.stdHandler $ getWalletBalance
        L.stdHandler startForeverChainGenerationHandler
        L.stdHandler generateBlocksPacketHandler
        L.stdHandler $ L.stopNodeHandler' stateVar
        L.stdHandler getLengthOfChain
        L.stdHandler ping
        L.stdHandler stopRequest
        L.stdHandler getBlock
        L.stdHandler $ createTransaction role
    L.awaitNodeFinished' stateVar

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a

eitherToText2 :: Either Text Text -> Text
eitherToText2 (Left  a) = "Server error: " <> a
eitherToText2 (Right a) = a
