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
import           Enecuum.Framework.Language.Extra (NodeStatus (..))
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude                  hiding (map, unpack)

type BlocksCount = Int

data CreateTransaction              = CreateTransaction CLITransaction D.Address deriving ( Generic, Show, Eq, Ord, Read, ToJSON)
newtype GetLastKBlock               = GetLastKBlock D.Address deriving Read
data GetWalletBalance               = GetWalletBalance Int D.Address deriving Read
newtype GetLengthOfChain            = GetLengthOfChain D.Address deriving Read
newtype StartForeverChainGeneration = StartForeverChainGeneration D.Address deriving Read
data GenerateBlocksPacket           = GenerateBlocksPacket BlocksCount D.Address deriving Read
data Ping                           = Ping Protocol D.Address deriving Read
newtype StopRequest                 = StopRequest D.Address deriving Read
data GetBlock                       = GetBlock D.StringHash D.Address deriving Read
data Protocol                       = UDP | TCP | RPC deriving (Generic, Show, Eq, Ord, FromJSON, Read)
data SendTo                         = SendTo Address D.PortNumber deriving Read
data Address                        = Address D.Host D.PortNumber deriving Read

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


sendTo :: SendTo -> L.NodeL Text
sendTo (SendTo (Address host port) rPort) = do
    void $ L.notify (D.Address host port) $ M.SendTo (D.toHashGeneric $ D.Address "127.0.0.1" rPort) 10 "!! msg !!"
    pure "Sended."

clientNode :: L.NodeDefinitionL ()
clientNode = do
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
        -- interaction with graph node sync scenario
        L.stdHandler getLastKBlockHandler
        L.stdHandler getLengthOfChain
        L.stdHandler getBlock

        -- interaction with poa node
        L.stdHandler startForeverChainGenerationHandler
        L.stdHandler generateBlocksPacketHandler

        -- routing sending
        L.stdHandler sendTo
    L.awaitNodeFinished' stateVar

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a

eitherToText2 :: Either Text Text -> Text
eitherToText2 (Left  a) = "Server error: " <> a
eitherToText2 (Right a) = a
