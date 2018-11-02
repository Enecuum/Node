{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Client where

import qualified Data.Aeson                       as J
import           Data.Aeson.Extra                 (noLensPrefix)
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Data.Text                        hiding (map)
import qualified Enecuum.Assets.Blockchain.Wallet as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import qualified Enecuum.Domain                   as D
import           Enecuum.Config
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude                  hiding (map, unpack)
import           Graphics.GD.Extra
import           Enecuum.Research.RouteDrawing
import           Data.Complex

data ClientNode = ClientNode
    deriving (Show, Generic)

data instance NodeConfig ClientNode = ClientNodeConfig
    { dummyOption :: Int
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

type BlocksCount = Int


data CreateTransaction'              = CreateTransaction' CLITransaction D.Address deriving ( Generic, Show, Eq, Ord, Read, ToJSON)
newtype GetLastKBlock'               = GetLastKBlock' D.Address deriving Read
data GetWalletBalance'               = GetWalletBalance' Int D.Address deriving Read
newtype GetLengthOfChain'            = GetLengthOfChain' D.Address deriving Read
newtype StartForeverChainGeneration' = StartForeverChainGeneration' D.Address deriving Read
data GenerateBlocksPacket'           = GenerateBlocksPacket' BlocksCount D.Address deriving Read
data Ping'                           = Ping' Protocol' D.Address deriving Read
newtype StopRequest'                 = StopRequest' D.Address deriving Read
data GetBlock'                       = GetBlock' D.StringHash D.Address deriving Read
data Protocol'                       = UDP | TCP | RPC deriving (Generic, Show, Eq, Ord, FromJSON, Read)
data SendTo'                         = SendTo' Address' D.PortNumber deriving Read
data Address'                        = Address' D.Host D.PortNumber deriving Read
newtype DrawMap'                     = DrawMap' Address' deriving Read
type Transmitter                     = D.Address
type Receiver                        = D.PortNumber


data CLITransaction = CLITransaction
  { _owner    :: String
  , _receiver :: String
  , _amount   :: D.Amount
  , _currency :: D.Currency
  } deriving ( Generic, Show, Eq, Ord, Read, Serialize)


instance ToJSON CLITransaction where toJSON = genericToJSON noLensPrefix
instance FromJSON CLITransaction where parseJSON = genericParseJSON noLensPrefix

instance J.FromJSON Ping' where
    parseJSON = J.withObject "Ping" $ \o -> Ping' <$> (o J..: "protocol") <*> (o J..: "address")

instance J.FromJSON GetWalletBalance' where
    parseJSON = J.withObject "GetWalletBalance" $ \o -> GetWalletBalance' <$> o J..: "walletID" <*> (o J..: "address")

instance J.FromJSON CreateTransaction' where
    parseJSON = J.withObject "CreateTransaction" $ \o -> CreateTransaction' <$> o J..: "tx" <*> (o J..: "address")

instance J.FromJSON GetLastKBlock' where
    parseJSON = J.withObject "GetLastKBlock" $ \o -> GetLastKBlock' <$> (o J..: "address")

instance J.FromJSON GetLengthOfChain' where
    parseJSON = J.withObject "GetLengthOfChain" $ \o -> GetLengthOfChain' <$> (o J..: "address")

instance J.FromJSON StartForeverChainGeneration' where
    parseJSON = J.withObject "StartForeverChainGeneration" $ \o -> StartForeverChainGeneration' <$> (o J..: "address")

instance J.FromJSON GenerateBlocksPacket' where
    parseJSON = J.withObject "GenerateBlocksPacket" $ \o -> GenerateBlocksPacket' <$> o J..: "blocks" <*> (o J..: "address")

instance J.FromJSON StopRequest' where
    parseJSON = J.withObject "StopRequest" $ \o -> StopRequest' <$> (o J..: "address")

instance J.FromJSON GetBlock' where
    parseJSON = J.withObject "GetBlock" $ \o -> GetBlock' <$> o J..: "hash" <*> (o J..: "address")

sendSuccessRequest :: forall a. (ToJSON a, Typeable a) => D.Address -> a -> L.NodeL Text
sendSuccessRequest address request = do
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address request
    pure . eitherToText $ res

startForeverChainGenerationHandler :: StartForeverChainGeneration' -> L.NodeL Text
startForeverChainGenerationHandler (StartForeverChainGeneration' address) = sendSuccessRequest address M.ForeverChainGeneration

generateBlocksPacketHandler :: GenerateBlocksPacket' -> L.NodeL Text
generateBlocksPacketHandler (GenerateBlocksPacket' i address) = sendSuccessRequest address $ M.NBlockPacketGeneration i

getLastKBlockHandler :: GetLastKBlock' -> L.NodeL Text
getLastKBlockHandler (GetLastKBlock' address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    pure . eitherToText $ res

idToKey :: (L.FileSystem m, Monad m) => Int -> m (D.PublicKey, String)
idToKey n = do
    let walletsMap = Map.fromList $ fmap (\w -> (A._id (w :: A.CLIWallet), w))  A.hardcodedWalletsWithNames
    case Map.lookup n walletsMap of
        Nothing -> error $ "There is no wallet with id: " +| n |+ ""
        Just j  -> pure (A._publicKey (j :: A.CLIWallet), A._name (j :: A.CLIWallet))

getWalletBalance :: GetWalletBalance' -> L.NodeL Text
getWalletBalance (GetWalletBalance' walletId address) = do
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

createTransaction :: CreateTransaction' -> L.NodeL Text
createTransaction (CreateTransaction' tx address) = do
    transaction <- transform tx
    res :: Either Text M.SuccessMsg <- L.makeRpcRequest address (M.CreateTransaction transaction)
    pure . eitherToText $ res

getLengthOfChain :: GetLengthOfChain' -> L.NodeL Text
getLengthOfChain (GetLengthOfChain' address) = do
    res :: Either Text D.KBlock <- L.makeRpcRequest address M.GetLastKBlock
    case res of
        Right kBlock -> pure $ "Length of chain is " <> show (D._number kBlock)
        Left t       -> pure t

ping :: Ping' -> L.NodeL Text
ping (Ping' TCP address) = do
    res <- L.withConnection D.Tcp address $ \conn -> L.send conn M.Ping
    pure $ case res of Right _ -> "Tcp port is available."; Left _ -> "Tcp port is not available."

ping (Ping' RPC address) = do
    res :: Either Text M.Pong <- L.makeRpcRequest address M.Ping
    pure $ case res of Right _ -> "Rpc port is available."; Left _ -> "Rpc port is not available."

ping (Ping' UDP _) = pure "This functionality is not supported."

stopRequest :: StopRequest' -> L.NodeL Text
stopRequest (StopRequest' address) = sendSuccessRequest address M.Stop

getBlock :: GetBlock' -> L.NodeL Text
getBlock (GetBlock' hash address) = do
    res :: Either Text D.NodeContent <- L.makeRpcRequest address (M.GetGraphNode hash)
    case res of
        Right (D.KBlockContent block) -> pure $ "Key block is "   <> show block
        Right (D.MBlockContent block) -> pure $ "Microblock is " <> show block
        Left  text                    -> pure $ "Error: "         <> text


sendTo :: SendTo' -> L.NodeL Text
sendTo (SendTo' (Address' host port) rPort) = do
    let receiver = D.Address "127.0.0.1" rPort
    void $ L.notify (D.Address host port) $ M.SendTo (D.toHashGeneric receiver) 10 "!! msg !!"
    pure "Sended."

drawRouteMap :: DrawMap' -> L.NodeL Text
drawRouteMap (DrawMap' (Address' host port)) = do
    routMap <- cardAssembly mempty mempty (Set.fromList [D.Address host port])
    L.evalIO $ makeImage (1000, 1000) "image.png" $ \image ->
        forM_ (Map.toList routMap) $ \(hs, hf) -> do
            let startPointPhase = hashToPhase hs
            let startPoint      = (500 :+ 500) + mkPolar 1 startPointPhase * 400
            forM_ (hashToPhase <$> hf) $ \ph -> do
                let endPoint = (500 :+ 500) + mkPolar 1 ph * 400
                drawCirkle endPoint 10 black image
                drawArrow startPoint endPoint black image
    pure "Drawed."

cardAssembly
    :: Map D.StringHash [D.StringHash]
    -> Set.Set D.Address
    -> Set.Set D.Address
    -> L.NodeL (Map D.StringHash [D.StringHash])
cardAssembly accum passed nexts
    | Set.null nexts = pure accum
    | otherwise      = do
        let D.Address host port = Set.elemAt 0 nexts
        res :: Either Text [(D.StringHash, D.Address)] <-
            L.makeRpcRequest (D.Address host (port - 1000)) M.ConnectMapRequest
        let r = case res of Right a -> a; Left _ -> []
        let newPassed = Set.insert (D.Address host port) passed
        let newNexts  = Set.difference (Set.union nexts (Set.fromList $ snd <$> r)) newPassed
        let newAccum  = Map.insert (D.toHashGeneric (D.Address host port)) (fst <$> r) accum
        cardAssembly newAccum newPassed newNexts

clientNode :: NodeConfig ClientNode -> L.NodeDefinitionL ()
clientNode _ = do
    L.logInfo "Client started"
    L.nodeTag "Client"
    stateVar <- L.newVarIO NodeActing
     -- -- status <- L.newVarIO NodeActing
     -- -- let nodeData = ClientNodeData status
     -- -- nodeData <- L.scenario $ L.atomically (ClientData1 <$> L.newVar D.genesisKBlock <*> L.newVar NodeActing <*> L.newVar [])

    L.std $ do
        -- network
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

        -- interaction with pow node
        L.stdHandler startForeverChainGenerationHandler
        L.stdHandler generateBlocksPacketHandler

        -- routing
        L.stdHandler sendTo
        L.stdHandler drawRouteMap
    L.awaitNodeFinished' stateVar

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a

eitherToText2 :: Either Text Text -> Text
eitherToText2 (Left  a) = "Server error: " <> a
eitherToText2 (Right a) = a
