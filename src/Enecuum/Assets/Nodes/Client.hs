{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}

module Enecuum.Assets.Nodes.Client where

import qualified Data.Aeson                       as J
import           Data.Aeson.Extra                 (noLensPrefix)
import qualified Data.Map                         as Map
import           Data.Text                        hiding (map)
import qualified Enecuum.Assets.Blockchain.Wallet as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import qualified Enecuum.Domain                   as D
import           Enecuum.Config
import           Enecuum.Framework.Language.Extra (NodeStatus (..))
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude                  hiding (map, unpack)

data ClientNode = ClientNode
    deriving (Show, Generic)

data instance NodeConfig ClientNode = ClientNodeConfig
    { _dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node ClientNode where
    data NodeScenario ClientNode = CLI
        deriving (Show, Generic)
    getNodeScript CLI = clientNode

instance ToJSON   ClientNode                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON ClientNode                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig ClientNode)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig ClientNode)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario ClientNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario ClientNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

type TimeGap = Int

data CreateTransaction              = CreateTransaction CLITransaction D.Address deriving ( Generic, Show, Eq, Ord, ToJSON)
newtype GetLastKBlock               = GetLastKBlock D.Address
data GetWalletBalance               = GetWalletBalance Int D.Address
newtype GetLengthOfChain            = GetLengthOfChain D.Address
newtype StartForeverChainGeneration = StartForeverChainGeneration D.Address
data GenerateBlocksPacket           = GenerateBlocksPacket
    { blocks  :: D.BlockNumber
    , timeGap :: TimeGap
    , address :: D.Address
    }
    deriving (Generic)

data Ping                           = Ping Protocol D.Address
newtype StopRequest                 = StopRequest D.Address
data GetBlock                       = GetBlock D.StringHash D.Address
data Protocol                       = UDP | TCP | RPC deriving (Generic, Show, Eq, Ord, FromJSON)
data DumpToDB                       = DumpToDB D.Address

data CLITransaction = CLITransaction
  { _owner    :: String
  , _receiver :: String
  , _amount   :: D.Amount
  , _currency :: D.Currency
  } deriving ( Generic, Show, Eq, Ord, Read, Serialize)


instance ToJSON CLITransaction where toJSON = genericToJSON noLensPrefix
instance FromJSON CLITransaction where parseJSON = genericParseJSON noLensPrefix

instance J.FromJSON Ping where
    parseJSON = J.withObject "Ping" $ \o -> Ping <$> (o J..: "protocol") <*> (o J..: "address")

instance J.FromJSON GetWalletBalance where
    parseJSON = J.withObject "GetWalletBalance" $ \o -> GetWalletBalance <$> o J..: "walletID" <*> (o J..: "address")

instance J.FromJSON CreateTransaction where
    parseJSON = J.withObject "CreateTransaction" $ \o -> CreateTransaction <$> o J..: "tx" <*> (o J..: "address")

instance J.FromJSON DumpToDB where
    parseJSON = J.withObject "DumpToDB" $ \o -> DumpToDB <$> o J..: "address"

instance J.FromJSON GetLastKBlock where
    parseJSON = J.withObject "GetLastKBlock" $ \o -> GetLastKBlock <$> (o J..: "address")

instance J.FromJSON GetLengthOfChain where
    parseJSON = J.withObject "GetLengthOfChain" $ \o -> GetLengthOfChain <$> (o J..: "address")

instance J.FromJSON StartForeverChainGeneration where
    parseJSON = J.withObject "StartForeverChainGeneration" $ \o -> StartForeverChainGeneration <$> (o J..: "address")

-- instance J.FromJSON GenerateBlocksPacket where
--     parseJSON = J.withObject "GenerateBlocksPacket"
--         $ \o -> GenerateBlocksPacket
--             <$> (o J..: "blocks" )
--             <*> (o J..: "timeGap" )
--             <*> (o J..: "address")

instance J.FromJSON GenerateBlocksPacket

instance J.FromJSON StopRequest where
    parseJSON = J.withObject "StopRequest" $ \o -> StopRequest <$> (o J..: "address")

instance J.FromJSON GetBlock where
    parseJSON = J.withObject "GetBlock" $ \o -> GetBlock <$> o J..: "hash" <*> (o J..: "address")

sendSuccessRequest :: forall a. (ToJSON a, Typeable a) => D.Address -> a -> L.NodeL Text
sendSuccessRequest address request = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address request
    pure . eitherToText $ res

startForeverChainGenerationHandler :: StartForeverChainGeneration -> L.NodeL Text
startForeverChainGenerationHandler (StartForeverChainGeneration address) = sendSuccessRequest address M.ForeverChainGeneration

generateBlocksPacketHandler :: GenerateBlocksPacket -> L.NodeL Text
generateBlocksPacketHandler (GenerateBlocksPacket i timeGap address) = sendSuccessRequest address $ M.NBlockPacketGeneration i timeGap

getLastKBlockHandler :: GetLastKBlock -> L.NodeL Text
getLastKBlockHandler (GetLastKBlock address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure . eitherToText $ res

idToKey :: (L.FileSystem m, Monad m) => Int -> m (D.PublicKey, String)
idToKey n = do
    let walletsMap = Map.fromList $ fmap (\w -> (A._id (w :: A.CLIWallet), w))  A.hardcodedWalletsWithNames
    case Map.lookup n walletsMap of
        Nothing -> error $ "There is no wallet with id: " +| n |+ ""
        Just j  -> pure (A._publicKey (j :: A.CLIWallet), A._name (j :: A.CLIWallet))

getWalletBalance :: GetWalletBalance -> L.NodeL Text
getWalletBalance (GetWalletBalance walletId address) = do
    (pubKey, name) <- idToKey walletId
    res :: Either Text M.WalletBalanceMsg <- L.makeRpcRequest address (M.GetWalletBalance pubKey)
    let fun :: M.WalletBalanceMsg -> Text
        fun (M.WalletBalanceMsg _ balance) = "" +|| name ||+ " : " +|| balance ||+ ""
    pure $ eitherToText2 $ fmap fun res

transform :: (L.ERandom m, L.Logger m, L.FileSystem m, Monad m) => CLITransaction -> m D.Transaction
transform tx = do
    let walletsMap = Map.fromList $ fmap (\w -> (A._name (w :: A.CLIWallet), w))  A.hardcodedWalletsWithNames
    let ownerName  = _owner tx
    let receiverName = _receiver tx
    let mbOwner = Map.lookup ownerName walletsMap
    let mbReceiver = Map.lookup receiverName walletsMap

    let (ownerPub, ownerPriv) = case mbOwner of
                    Nothing -> error $ "There is no record for owner: " +|| ownerName ||+ "."
                    Just  j -> (pub, priv)
                                where pub = A._publicKey (j :: A.CLIWallet)
                                      priv = fromJust $ A._privateKey (j :: A.CLIWallet)
        receiverPub = case mbReceiver of
                        Nothing -> read receiverName :: D.PublicKey
                        Just j  -> A._publicKey (j :: A.CLIWallet)
    uuid <- L.nextUUID
    D.signTransaction ownerPub ownerPriv receiverPub (_amount tx) (_currency tx) uuid

createTransaction :: CreateTransaction -> L.NodeL Text
createTransaction (CreateTransaction tx address) = do
    transaction <- transform tx
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address (M.CreateTransaction transaction)
    pure . eitherToText $ res

dumpToDB :: DumpToDB -> L.NodeL Text
dumpToDB (DumpToDB address) = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address M.DumpToDB
    pure . eitherToText $ res

getLengthOfChain :: GetLengthOfChain -> L.NodeL Text
getLengthOfChain (GetLengthOfChain address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    case res of
        Right kBlock -> pure $ "Length of chain is " <> show (D._number kBlock)
        Left t       -> pure t

ping :: Ping -> L.NodeL Text
ping (Ping TCP address) = do
    res <- L.withConnection D.Tcp address $ \conn -> L.send conn M.Ping
    pure $ case res of Right _ -> "Tcp port is available."; Left _ -> "Tcp port is not available."

ping (Ping RPC address) = do
    res :: Either Text M.Pong <- L.makeRpcRequest address M.Ping
    pure $ case res of Right _ -> "Rpc port is available."; Left _ -> "Rpc port is not available."

ping (Ping UDP _) = pure "This functionality is not supported."

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
{"method":"GenerateBlocksPacket", "blocks" : 2, "timeGap":0, "address":{"host":"127.0.0.1", "port": 2005}}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"GetWalletBalance", "walletID": 2, "address":{"host":"127.0.0.1", "port": 2009}}
{"method":"StopNode"}
{"method":"Ping", "protocol":"RPC", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"GetLengthOfChain", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"StopRequest", "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"CreateTransaction", "tx": {"amount":15, "owner": "me", "receiver":"Alice","currency": "ENQ"}, "address":{"host":"127.0.0.1", "port": 2008}}
{"method":"DumpToDB", "address":{"host":"127.0.0.1", "port": 2008}}
-}

clientNode :: NodeConfig ClientNode -> L.NodeDefinitionL ()
clientNode _ = do
    L.logInfo "Client started"
    L.nodeTag "Client"
    stateVar <- L.newVarIO NodeActing
    L.std $ do
        -- interaction with any node
        L.stdHandler ping
        L.stdHandler stopRequest
        L.stdHandler $ L.stopNodeHandler' stateVar

        -- interaction with graph node
        L.stdHandler createTransaction
        L.stdHandler getWalletBalance
        L.stdHandler dumpToDB
        -- interaction with graph node sync scenario
        L.stdHandler getLastKBlockHandler
        L.stdHandler getLengthOfChain
        L.stdHandler getBlock

        -- interaction with poa node
        L.stdHandler startForeverChainGenerationHandler
        L.stdHandler generateBlocksPacketHandler
    L.awaitNodeFinished' stateVar

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a

eitherToText2 :: Either Text Text -> Text
eitherToText2 (Left  a) = "Server error: " <> a
eitherToText2 (Right a) = a
