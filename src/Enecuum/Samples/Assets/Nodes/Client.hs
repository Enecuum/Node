{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
module Enecuum.Samples.Assets.Nodes.Client
  ( clientNode
  , ClientNode(..)
  , NodeConfig (..)
  , SendTo(..)
  , Protocol(..)
  ) where

import qualified Data.Aeson                               as J
import           Data.Aeson.Extra                         (noLensPrefix)
import           Data.Complex
import qualified Data.Map                                 as Map
import           Data.Set                                 (union, (\\))
import qualified Data.Set                                 as Set
import           Data.Text                                hiding (filter, map)
import           Enecuum.Config
import qualified Enecuum.Domain                           as D
import           Enecuum.Framework.Domain.Error
import qualified Enecuum.Framework.Lens                   as Lens
import qualified Enecuum.Language                         as L
import           Enecuum.Prelude                          hiding (unpack)
import           Enecuum.Samples.Assets.Blockchain.Keys
import qualified Enecuum.Samples.Assets.Blockchain.Wallet as A
import qualified Enecuum.Samples.Assets.Nodes.Address     as A
import qualified Enecuum.Samples.Assets.Nodes.Messages    as M
import qualified Enecuum.Samples.Blockchain.Domain        as D
import qualified Enecuum.Samples.Blockchain.Language      as L
import           System.Console.Haskeline
import           Text.Regex.Posix                         ((=~))

data ClientNode = ClientNode
    deriving (Show, Generic)

data instance NodeConfig ClientNode = ClientNodeConfig
    { _dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node ClientNode where
    data NodeScenario ClientNode = CLI
        deriving (Show, Generic)
    getNodeScript CLI = clientNode'
    getNodeTag _ = ClientNode

instance ToJSON   ClientNode                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON ClientNode                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig ClientNode)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig ClientNode)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario ClientNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario ClientNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

type TimeGap = Int

data CreateTransaction              = CreateTransaction CLITransaction D.Address deriving (Show, Eq, Ord, Read)
newtype GetLastKBlock               = GetLastKBlock D.Address deriving Read
data GetWalletBalance               = GetWalletBalance Int D.Address deriving Read
newtype GetLengthOfChain            = GetLengthOfChain D.Address deriving Read
newtype StartForeverChainGeneration = StartForeverChainGeneration D.Address deriving Read
data Ping                           = Ping Protocol D.Address deriving Read
newtype StopRequest                 = StopRequest D.Address deriving Read
data GetBlock                       = GetBlock D.StringHash D.Address deriving Read
data Protocol                       = UDP | TCP | RPC deriving (Show, Eq, Ord, Read)
data SendTo                         = SendTo Address D.PortNumber deriving Read
data Address                        = Address D.Host D.PortNumber deriving Read

data GenerateBlocksPacket           = GenerateBlocksPacket
    { blocks  :: D.BlockNumber
    , timeGap :: TimeGap
    , address :: D.Address
    }
    deriving (Show, Read)

data DumpToDB = DumpToDB
    { address :: D.Address
    }
    deriving (Show, Read)

data RestoreFromDB = RestoreFromDB
    { address :: D.Address
    }
    deriving (Show, Read)

data CLITransaction = CLITransaction
  { _owner    :: String
  , _receiver :: String
  , _amount   :: D.Amount
  , _currency :: D.Currency
  } deriving (Show, Eq, Ord, Read)

sendSuccessRequest :: forall a. (ToJSON a, Typeable a) => D.Address -> a -> L.NodeL Text
sendSuccessRequest address request = do
    res :: Either Text D.SuccessMsg <- L.makeRpcRequest address request
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
    res :: Either Text D.SuccessMsg <- L.makeRpcRequest address (M.CreateTransaction transaction)
    pure . eitherToText $ res

dumpToDB :: DumpToDB -> L.NodeL Text
dumpToDB (DumpToDB address) = do
    res :: Either Text D.SuccessMsg <- L.makeRpcRequest address M.DumpToDB
    pure . eitherToText $ res

restoreFromDB :: RestoreFromDB -> L.NodeL Text
restoreFromDB (RestoreFromDB address) = do
    res :: Either Text D.SuccessMsg <- L.makeRpcRequest address M.RestoreFromDB
    pure . eitherToText $ res

getLengthOfChain :: GetLengthOfChain -> L.NodeL Text
getLengthOfChain (GetLengthOfChain address) = do
    mKBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure $ case mKBlock of
        Right kBlock       -> "Length of chain is " <> show (D._number kBlock)
        Left  requestError -> requestError

ping :: Ping -> L.NodeL Text
ping (Ping TCP address) = do
    ok <- L.withConnection D.Tcp address $ \conn -> L.send conn D.Ping
    pure $ case ok of
        Just (Right _) -> "Tcp port is available."
        Just (Left  _) -> "Tcp disconnection."
        _              -> "Tcp port is not available."

ping (Ping RPC address) = do
    res :: Either Text D.Pong <- L.makeRpcRequest address D.Ping
    pure $ case res of Right _ -> "Rpc port is available."; Left _ -> "Rpc port is not available."

ping (Ping UDP _) = pure "This functionality is not supported."

stopRequest :: StopRequest -> L.NodeL Text
stopRequest (StopRequest address) = sendSuccessRequest address D.Stop

getBlock :: GetBlock -> L.NodeL Text
getBlock (GetBlock hash address) = do
    mblock <- L.makeRpcRequest address (M.GetGraphNode hash)
    pure $ case mblock of
        Right (D.KBlockContent block) -> "Key block is "  <> show block
        Right (D.MBlockContent block) -> "Microblock is " <> show block
        Left  requestError            -> "Error: "        <> requestError

createNodeId :: M.CreateNodeId -> L.NodeL Text
createNodeId (M.CreateNodeId password) = do
    createKeyPair NodeId $ User (Manual password)
    pure "Success"

createWallet :: M.CreateWallet -> L.NodeL Text
createWallet (M.CreateWallet password) = do
    createWallet' "default" $ User (Manual password)
    pure "Success"

createWalletWithAlias :: M.CreateWalletWithAlias -> L.NodeL Text
createWalletWithAlias (M.CreateWalletWithAlias alias (M.CreateWallet password)) = do
    createWallet' alias $ User (Manual password)
    pure "Success"

showMyWallets :: M.ShowMyWallets -> L.NodeL Text
showMyWallets _ = showWallets


-- TODO change it to console help
help :: Help -> L.NodeL Text
help Help = pure "TODO change it to console help"
{- Commands for client:
CreateNodeId  'Password'
CreateWallet  'Password'
ShowMyWallets
CreateWalletWithAlias "alias" (CreateWallet "Password")
-}

-- For CLI completion
completeWith :: [L.CLICommand] -> String -> [Completion]
completeWith possibles left = case filter (=~ left) possibles of
  []  -> []
  [x] -> [Completion x x False]
  xs  -> map (\str -> Completion left str False) xs

-- local activity
data Help = Help deriving (Show, Read)
data CmdLocal = CreateNodeId | CreateWallet | CreateWalletWithAlias | ShowMyWallets deriving (Show, Enum, Bounded)
cliCommandsLocal = fmap show localCmds
    where localCmds = [minBound..maxBound] :: [CmdLocal]

cliCommands = "Help" : cliCommandsLocal


clientNode :: L.NodeDefinitionL ()
clientNode = clientNode' (ClientNodeConfig 42)

clientNode' :: NodeConfig ClientNode -> L.NodeDefinitionL ()
clientNode' _ = do
    L.logInfo "Client started"
    L.setNodeTag "Client"
    stateVar <- L.newVarIO D.NodeActing

    L.stdF (completeWith cliCommands) $ do
        -- network
        L.stdHandler ping
        L.stdHandler stopRequest
        L.stdHandler $ L.stopNodeHandler' stateVar

        -- local activity
        L.stdHandler createNodeId
        L.stdHandler createWallet
        L.stdHandler createWalletWithAlias
        L.stdHandler showMyWallets
        L.stdHandler help

        -- interaction with graph node
        L.stdHandler createTransaction
        L.stdHandler getWalletBalance
        L.stdHandler dumpToDB
        L.stdHandler restoreFromDB
        -- interaction with graph node sync scenario
        L.stdHandler getLastKBlockHandler
        L.stdHandler getLengthOfChain
        L.stdHandler getBlock

        -- interaction with pow node
        L.stdHandler startForeverChainGenerationHandler

        --  GenerateBlocksPacket {blocks = 2, timeGap = 0, address = Address {_host = "127.0.0.1", _port = 6020}}
        L.stdHandler generateBlocksPacketHandler

    L.awaitNodeFinished' stateVar
