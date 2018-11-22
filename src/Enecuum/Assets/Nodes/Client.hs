{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.Client (clientNode, ClientNode(..), NodeConfig (..), SendTo(..), sendTo, Protocol(..)) where

import qualified Data.Aeson                            as J
import           Data.Aeson.Extra                      (noLensPrefix)
import           Data.Complex
import qualified Data.Map                              as Map
import           Data.Set                              (union, (\\))
import qualified Data.Set                              as Set
import           Data.Text                             hiding (map)
import           Enecuum.Assets.Blockchain.Keys
import qualified Enecuum.Assets.Blockchain.Wallet      as A
import qualified Enecuum.Assets.Nodes.Address          as A
import qualified Enecuum.Assets.Nodes.Messages         as M
import           Enecuum.Assets.Nodes.Routing.Messages
import           Enecuum.Config
import qualified Enecuum.Domain                        as D
import           Enecuum.Framework.Domain.Error
import           Enecuum.Framework.Language.Extra      (NodeStatus (..))
import qualified Enecuum.Language                      as L
import           Enecuum.Prelude                       hiding (map, unpack)
import           Enecuum.Research.RouteDrawing
import           Graphics.GD.Extra

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
newtype DrawMap                     = DrawMap D.PortNumber deriving Read

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

restoreFromDB :: RestoreFromDB -> L.NodeL Text
restoreFromDB (RestoreFromDB address) = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address M.RestoreFromDB
    pure . eitherToText $ res

getLengthOfChain :: GetLengthOfChain -> L.NodeL Text
getLengthOfChain (GetLengthOfChain address) = do
    mKBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure $ case mKBlock of
        Right kBlock       -> "Length of chain is " <> show (D._number kBlock)
        Left  requestError -> requestError

ping :: Ping -> L.NodeL Text
ping (Ping TCP address) = do
    ok <- L.withConnection D.Tcp address $ \conn -> L.send conn M.Ping
    pure $ case ok of
        Just (Right _) -> "Tcp port is available."
        Just (Left  _) -> "Tcp disconnection."
        _              -> "Tcp port is not available."

ping (Ping RPC address) = do
    res :: Either Text M.Pong <- L.makeRpcRequest address M.Ping
    pure $ case res of Right _ -> "Rpc port is available."; Left _ -> "Rpc port is not available."

ping (Ping UDP _) = pure "This functionality is not supported."

stopRequest :: StopRequest -> L.NodeL Text
stopRequest (StopRequest address) = sendSuccessRequest address M.Stop

getBlock :: GetBlock -> L.NodeL Text
getBlock (GetBlock hash address) = do
    mblock <- L.makeRpcRequest address (M.GetGraphNode hash)
    pure $ case mblock of
        Right (D.KBlockContent block) -> "Key block is "  <> show block
        Right (D.MBlockContent block) -> "Microblock is " <> show block
        Left  requestError            -> "Error: "        <> requestError


sendTo :: SendTo -> L.NodeL Text
sendTo (SendTo (Address host port) rPort) = do
    let receiverHash    = D.toHashGeneric $ A.makeNodePorts1000 rPort
    let resenderUdpPort = A.makeNodePorts1000 port ^. A.nodeUdpPort
    let resenderAddress = D.Address host resenderUdpPort
    ok <- L.notify resenderAddress $ SendMsgTo receiverHash 10 "!! msg !!"
    if isRight ok
        then pure   "Test message is sent."
        else pure $ "Error of sending: " <> show ok

drawRouteMap :: DrawMap -> L.NodeL Text
drawRouteMap (DrawMap port) = do
    let startAddress = A.makeAddressByPorts $ A.makeNodePorts1000 port
    routMap <- cardAssembly mempty mempty (Set.fromList [startAddress])
    L.evalIO $ makeImage (1000, 1000) "image.png" $ \image ->
        forM_ (Map.toList routMap) $ \(hs, hf) -> do
            let startPointPhase = hashToPhase hs
            let startPoint      = (500 :+ 500) + mkPolar 1 startPointPhase * 400
            forM_ (hashToPhase <$> hf) $ \ph -> do
                let endPoint = (500 :+ 500) + mkPolar 1 ph * 400
                drawCircle endPoint 10 black image
                drawArrow startPoint endPoint black image
    pure "Drawn."

-- | Build connection map.
cardAssembly
    :: Map D.StringHash [D.StringHash]
    -> Set.Set A.NodeAddress
    -> Set.Set A.NodeAddress
    -> L.NodeL (Map D.StringHash [D.StringHash])
cardAssembly accum passed nexts
    | Set.null nexts = pure accum
    | otherwise      = do
        -- take the address of who will ask the next contact
        let currentAddress = Set.elemAt 0 nexts
        connects <- fromRight [] <$>
            L.makeRpcRequest (A.getRpcAddress currentAddress) M.ConnectMapRequest

        -- add the address to the list of the passed
        let newPassed :: Set.Set A.NodeAddress
            newPassed = Set.insert currentAddress passed

        -- add received addresses to the queue and remove from it those that have already visited
        let newNexts :: Set.Set A.NodeAddress
            newNexts  = (nexts `union` Set.fromList connects) \\ newPassed

        -- add to the accumulator addresses for the passed address
        let newAccum :: Map.Map D.StringHash [D.StringHash]
            newAccum  = Map.insert (currentAddress ^. A.nodeId) ((^. A.nodeId) <$> connects) accum
        cardAssembly newAccum newPassed newNexts

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
{- Commands for client:
CreateNodeId "Password"
CreateWallet "Password"
ShowMyWallets
CreateWalletWithAlias "alias" (CreateWallet "Password")
-}

clientNode :: L.NodeDefinitionL ()
clientNode = clientNode' (ClientNodeConfig 42)

clientNode' :: NodeConfig ClientNode -> L.NodeDefinitionL ()
clientNode' _ = do
    L.logInfo "Client started"
    L.nodeTag "Client"
    stateVar <- L.newVarIO NodeActing

    L.std $ do
        -- network
        L.stdHandler ping
        L.stdHandler stopRequest
        L.stdHandler $ L.stopNodeHandler' stateVar

        -- local activity
        L.stdHandler createNodeId
        L.stdHandler createWallet
        L.stdHandler createWalletWithAlias
        L.stdHandler showMyWallets

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

        -- routing
        L.stdHandler sendTo
        L.stdHandler drawRouteMap
    L.awaitNodeFinished' stateVar
