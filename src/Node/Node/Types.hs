{-# LANGUAGE GADTs, DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module provides types for storing internal state of a node and messages.
-- Different nodes use mutually overlapping set of messages
module Node.Node.Types where


import              Service.Types (Transaction, Microblock)
import              Network.Socket
import              Sharding.Types.Node
import              Node.Template.Constructor
import              System.Clock
import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import qualified    Data.Map                        as M
import qualified    Crypto.PubKey.ECC.DH            as DH

import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges
import              Node.Data.Lens

import qualified    Data.Bimap                      as BI
import              Crypto.PubKey.ECC.ECDSA         as ECDSA
import              Crypto.PubKey.ECC.Generate
import              Crypto.Random.Types
import              Data.Serialize
import              Data.Monoid
import              Lens.Micro

import              Control.Concurrent.Chan
import              GHC.Generics (Generic)
import              Node.Crypto
import              Node.Data.Data

import              Sharding.Types.Node
import              Sharding.Space.Point

instance Show (Chan a) where
    show _ = "Chan"

data Msg where Msg :: B.ByteString -> Msg
type Transactions = [Transaction]
data Answer where
    StateRequestAnswer ::
        NodeVariantRoles
        -> MyNodeId
        -> Int
        -> Int
        -> Int
        -> Answer
    RawPackege :: B.ByteString -> Answer
  deriving Show

data ExitMsg where
    ExitMsg :: ExitMsg

data MsgToSender where
    MsgToSender     :: B.ByteString -> MsgToSender
    SenderExit      :: B.ByteString -> MsgToSender
    SenderTerminate :: MsgToSender

dataConstruct "MsgToNodeManager" $
    ((_1 .~ False) <$> managerMsgFuncListData) <>
    ((_1 .~ False) <$> managerMiningMsgListData)

dataConstruct "ManagerMsgBase" managerMsgFuncListData

dataConstruct "ManagerMiningMsgBase" $
    ((_1 .~ False) <$> managerMsgFuncListData) <>
    managerMiningMsgListData

msgClass []             "ManagerMsg" managerMsgFuncListFull
msgClass ["ManagerMsg"] "ManagerMiningMsg" managerMiningMsgListFull


baseMsgInstance "ManagerMsg" "ManagerMsgBase" managerMsgFuncList
baseMsgInstance "ManagerMiningMsg" "ManagerMiningMsgBase" managerMiningMsgList


derivativeMsgInstance "ManagerMsg" "MsgToNodeManager" managerMsgFuncList
derivativeMsgInstance "ManagerMsg" "ManagerMiningMsgBase" managerMsgFuncList

derivativeMsgInstance "ManagerMiningMsg" "MsgToNodeManager" managerMiningMsgList

data MsgToServer where
    KillMsg       :: MsgToServer

data NodeStatus = Active | Noactive deriving (Show, Eq)

data Node = Node {
    nodeStatus          :: NodeStatus,
    nodeKey             :: Maybe StringKey,
    nodeChan            :: Chan MsgToSender,
    nodeNodePosition    :: Maybe NodePosition,
    nodeIsBroadcast     :: Bool
  }


data ManagerNodeData where
    ManagerNodeData :: {
        managerNodeDataNodeConfig   :: NodeConfig,
        managerNodeDataNodeBaseData :: NodeBaseData,
        managerTransactions         :: Chan Transaction,
        managerHashMap              :: BI.Bimap TimeSpec B.ByteString,
        managerPublicators          :: S.Set NodeId,
        managerSendedTransctions    :: BI.Bimap TimeSpec Transaction,
        managerShardingChan         :: Maybe (Chan ShardingNodeAction)
  } -> ManagerNodeData

type IdIpPort = (NodeId, HostAddress, PortNumber)
type IpPort = (HostAddress, PortNumber)

data NodeBaseData where
    NodeBaseData :: {
        nodeBaseDataExitChan            :: Chan ExitMsg,
        nodeBaseDataNodes               :: M.Map NodeId Node,
        nodeBaseDataBootNodes           :: BootNodeList,
        nodeBaseDataAnswerChan          :: Chan Answer,
        nodeBaseDataVacantPositions     :: BI.Bimap TimeSpec IdIpPort,
        nodeBaseDataBroadcastNum        :: Int,
        nodeBaseDataHostAddress         :: Maybe HostAddress,
        nodeBaseDataMicroblockChan      :: Chan Microblock
  } -> NodeBaseData



data NodeConfig where
     NodeConfig :: {
        nodeConfigPrivateNumber :: DH.PrivateNumber,
        nodeConfigPublicPoint   :: DH.PublicPoint,
        nodeConfigPrivateKey    :: PrivateKey,
        nodeConfigPublicKey     :: PublicKey,
        nodeConfigMyNodeId      :: MyNodeId,
        nodeConfigHelloMsg      :: HelloMsg,
        nodeConfigPortNumber    :: PortNumber
  } -> NodeConfig
  deriving (Generic)


genDataClass        "nodeConfig" nodeConfigList
genBazeDataInstance "nodeConfig" (fst <$> nodeConfigList)

genDataClass        "nodeBaseData" nodeBaseDataList
genBazeDataInstance "nodeBaseData" (fst <$> nodeBaseDataList)

instance Serialize NodeConfig

class (NodeConfigClass a, NodeBaseDataClass a) => ManagerData a
instance ManagerData ManagerNodeData


mapM (uncurry makeLensInstance') [
    ("helloMsg", "nodeConfig"),
    ("nodeConfig", "managerNodeData"),
    ("nodeBaseData", "managerNodeData")]


instance Serialize PrivateKey where
    get = PrivateKey <$> get <*> get
    put (PrivateKey a b)= put a >> put b

class ToManagerData a where
    toManagerData ::
        Chan Transaction
        -> Chan Microblock
        -> Chan ExitMsg
        -> Chan Answer
        -> BootNodeList
        -> NodeConfig
        ->  a

instance ToManagerData ManagerNodeData where
    toManagerData aTransactionChan aMicroblockChan aExitChan aAnswerChan aList aNodeConfig = ManagerNodeData
        aNodeConfig (NodeBaseData aExitChan M.empty aList aAnswerChan BI.empty 0 Nothing aMicroblockChan)
            aTransactionChan BI.empty S.empty BI.empty Nothing

defaultHelloMsg :: HelloMsg
defaultHelloMsg = HelloMsg (P2pVersion 0) (ClientId 0) 3000 (NodeId 0) []
    [SimpleNode]


makeNewNodeConfig :: MonadRandom m => PortNumber -> m NodeConfig
makeNewNodeConfig aPort = do
    (aPublicKey,     aPrivateKey)  <- generate curve
    (aPrivateNumber, aPublicPoint) <- genKayPair curve
    let aId = keyToId aPublicKey
    pure $ NodeConfig aPrivateNumber aPublicPoint aPrivateKey aPublicKey
        (toMyNodeId aId) (defaultHelloMsg & nodeId .~ aId) aPort


emptyData :: (MonadRandom m, ToManagerData d) =>
    PortNumber
    -> Chan Transaction
    -> Chan Microblock
    -> Chan ExitMsg
    -> Chan Answer
    -> BootNodeList
    -> m d
emptyData aPort aTransactionChan aMicroblockChan aExitChan aAnswerChan aList =
    toManagerData aTransactionChan aMicroblockChan aExitChan aAnswerChan  aList
        <$> makeNewNodeConfig aPort


roles :: NodeConfigClass a => Lens' a NodeVariantRoles
roles = nodeConfig.helloMsg.nodeVariantRoles


lensInst "mKey"         ["Node"] ["Maybe", "StringKey"]         "nodeKey"
lensInst "chan"         ["Node"] ["Chan", "MsgToSender"]        "nodeChan"
lensInst "status"       ["Node"] ["NodeStatus"]                 "nodeStatus"
lensInst "nodePosition" ["Node"] ["Maybe", "NodePosition"]      "nodeNodePosition"
lensInst "isBroadcast"  ["Node"] ["Bool"]                       "nodeIsBroadcast"


lensInst "transactions" ["ManagerNodeData"]
    ["Chan", "Transaction"] "managerTransactions"

lensInst "hashMap" ["ManagerNodeData"]
    ["BI.Bimap", "TimeSpec", "B.ByteString"] "managerHashMap"

lensInst "publicators" ["ManagerNodeData"] ["S.Set", "NodeId"]
    "managerPublicators"

lensInst "sendedTransctions" ["ManagerNodeData"]
    ["BI.Bimap", "TimeSpec", "Transaction"] "managerSendedTransctions"

type MaybeChan a = Maybe (Chan a)

lensInst "shardingChan" ["ManagerNodeData"] ["MaybeChan", "ShardingNodeAction"] "managerShardingChan"

makeNode :: Chan MsgToSender -> Node
makeNode aChan = Node {
    nodeStatus          = Noactive,
    nodeKey             = Nothing,
    nodeChan            = aChan,
    nodeNodePosition    = Nothing,
    nodeIsBroadcast     = False
  }


defaultServerPort :: PortNumber
defaultServerPort = 3000
