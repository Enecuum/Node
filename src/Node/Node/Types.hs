{-# LANGUAGE
        GADTs
    ,   DeriveGeneric
    ,   TemplateHaskell
    ,   OverloadedStrings
    ,   TypeSynonymInstances
    ,   FlexibleInstances
    ,   MultiWayIf
    ,   MultiParamTypeClasses
#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module provides types for storing internal state of a node and messages.
-- Different nodes use mutually overlapping set of messages
module Node.Node.Types where

import              Network.Socket
import              System.Clock
import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import qualified    Data.Bimap                      as BI
import              Data.Serialize
import              Data.Monoid
import qualified    Data.Map                        as M
import qualified    Crypto.PubKey.ECC.DH            as DH
import              GHC.Generics (Generic)
import              Control.Concurrent.Chan
import              Crypto.Random.Types
import              Crypto.PubKey.ECC.ECDSA         as ECDSA
import              Crypto.PubKey.ECC.Generate
import              Lens.Micro
import              Lens.Micro.TH

import              Node.Crypto
import              Node.Data.Data
import              Node.Data.NetPackage
import              Node.Data.Lens
import              Node.Data.NodeTypes
import              Node.Template.Constructor
import              Sharding.Space.Point
import qualified    Sharding.Types.Node as N
import              Service.Types (Transaction, Microblock)
import              Sharding.Space.Distance

import              Data.Scientific (toRealFloat, Scientific)
import              Data.Aeson
import              Data.Aeson.TH
import              Service.InfoMsg
import              Service.Network.Base (ConnectInfo)



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
        _status          :: NodeStatus
    ,   _mKey            :: Maybe StringKey
    ,   _chan            :: Chan MsgToSender
    ,   _nodePosition    :: Maybe NodePosition
    ,   _nodePort        :: PortNumber
    ,   _isBroadcast     :: Bool
    ,   _nodeHost        :: HostAddress
  }
  deriving (Eq)

makeLenses ''Node

data ManagerNodeData = ManagerNodeData {
        managerNodeDataNodeConfig   :: NodeConfig
    ,   managerNodeDataNodeBaseData :: NodeBaseData
    ,   managerTransactions         :: Chan Transaction
    ,   managerHashMap              :: BI.Bimap TimeSpec B.ByteString
    ,   managerPublicators          :: S.Set NodeId
    ,   managerSendedTransctions    :: BI.Bimap TimeSpec Transaction
  }

--type IdIpPort = (NodeId, HostAddress, PortNumber)
type IpPort = (HostAddress, PortNumber)
type ShardingChan = Chan N.ShardingNodeAction
type MaybeChan a = Maybe (Chan a)

data NodeBaseData = NodeBaseData {
        nodeBaseDataExitChan            :: Chan ExitMsg
    ,   nodeBaseDataNodes               :: M.Map NodeId Node
    ,   nodeBaseDataBootNodes           :: BootNodeList
    ,   nodeBaseDataAnswerChan          :: Chan Answer
    ,   nodeBaseDataBroadcastNum        :: Int
    ,   nodeBaseDataHostAddress         :: Maybe HostAddress
    ,   nodeBaseDataMicroblockChan      :: Chan Microblock
    ,   nodeBaseDataMyNodePosition      :: Maybe MyNodePosition
    ,   nodeBaseDataShardingChan        :: MaybeChan N.ShardingNodeAction
    ,   nodeBaseDataIAmBroadcast        :: Bool
    ,   nodeBaseDataOutPort             :: PortNumber
    ,   nodeBaseDataInfoMsgChan         :: Chan InfoMsg
  }

makeNodeBaseData :: Chan ExitMsg
                 -> BootNodeList
                 -> Chan Answer
                 -> Chan Microblock
                 -> PortNumber
                 -> Chan InfoMsg
                 -> NodeBaseData
makeNodeBaseData aExitChan aList aAnswerChan aMicroblockChan port aInfoCh = NodeBaseData
    aExitChan
    M.empty
    aList
    aAnswerChan
    0
    Nothing
    aMicroblockChan
    Nothing
    Nothing
    False
    port
    aInfoCh

-- | TODO: Нужно отрефакторить, уменьшить колво ключей.
data NodeConfig = NodeConfig {
    nodeConfigPrivateNumber :: DH.PrivateNumber,
    nodeConfigPublicPoint   :: DH.PublicPoint,
    nodeConfigPrivateKey    :: PrivateKey,
    nodeConfigMyNodeId      :: MyNodeId
  }
  deriving (Generic)
$(deriveJSON defaultOptions ''NodeConfig)

data SimpleNodeBuildConfig where
     SimpleNodeBuildConfig :: {
        poaInPort      :: PortNumber,
        poaOutPort     :: PortNumber,
        rpcPort        :: PortNumber
  } -> SimpleNodeBuildConfig
  deriving (Generic)

instance ToJSON PortNumber where
  toJSON pn = Number $ fromInteger $ toInteger pn

toDouble :: Scientific -> Double
toDouble = toRealFloat

instance FromJSON PortNumber where
    parseJSON (Number s) = return.toEnum.fromEnum.toDouble $ s
    parseJSON _          = error "i've felt with the portnumber parsing"


$(deriveJSON defaultOptions ''SimpleNodeBuildConfig)

$(deriveJSON defaultOptions ''ConnectInfo)

data BuildConfig where
     BuildConfig :: {
        extConnectPort        :: PortNumber,
        bootNodeList          :: String,
        sharding              :: String,
        simpleNodeBuildConfig :: Maybe SimpleNodeBuildConfig,
        statsdBuildConfig     :: ConnectInfo,
        logsBuildConfig       :: ConnectInfo
  } -> BuildConfig
  deriving (Generic)

$(deriveJSON defaultOptions ''BuildConfig)



genDataClass        "nodeConfig" nodeConfigList
genBazeDataInstance "nodeConfig" (fst <$> nodeConfigList)

genDataClass        "nodeBaseData" nodeBaseDataList
genBazeDataInstance "nodeBaseData" (fst <$> nodeBaseDataList)

instance Serialize NodeConfig

class (NodeConfigClass a, NodeBaseDataClass a) => ManagerData a
instance ManagerData ManagerNodeData


mapM (uncurry makeLensInstance') [
        ("nodeConfig", "managerNodeData")
    ,   ("nodeBaseData", "managerNodeData")
    ]


instance Serialize PrivateKey where
    get = PrivateKey <$> get <*> get
    put (PrivateKey a b)= put a >> put b

class ToManagerData a where
    toManagerData
        :: Chan Transaction
        -> Chan Microblock
        -> Chan ExitMsg
        -> Chan Answer
        -> Chan InfoMsg
        -> BootNodeList
        -> NodeConfig
        -> PortNumber
        ->  a

instance ToManagerData ManagerNodeData where
    toManagerData aTransactionChan aMicroblockChan aExitChan aAnswerChan aInfoChan aList aNodeConfig aOutPort = ManagerNodeData
        aNodeConfig (makeNodeBaseData aExitChan aList aAnswerChan aMicroblockChan aOutPort aInfoChan)
            aTransactionChan BI.empty S.empty BI.empty


makeNewNodeConfig :: MonadRandom m => m NodeConfig
makeNewNodeConfig = do
    (aPublicKey,     aPrivateKey)  <- generate curve
    (aPrivateNumber, aPublicPoint) <- genKayPair curve
    let aId = keyToId aPublicKey
    pure $ NodeConfig aPrivateNumber aPublicPoint aPrivateKey (toMyNodeId aId)

-- FIXME: find a right place.
makePackageSignature
    ::  Serialize aPackage
    =>  ManagerData md
    =>  md
    ->  aPackage
    ->  IO PackageSignature
makePackageSignature aData aResponse = do
    aTime <- getTime Realtime
    let aNodeId = aData^.myNodeId
    aResponceSignature <- signEncodeble
        (aData^.privateKey)
        (aNodeId, aTime, aResponse)
    return $ PackageSignature aNodeId aTime aResponceSignature


lensInst "transactions" ["ManagerNodeData"]
    ["Chan", "Transaction"] "managerTransactions"

lensInst "hashMap" ["ManagerNodeData"]
    ["BI.Bimap", "TimeSpec", "B.ByteString"] "managerHashMap"

lensInst "publicators" ["ManagerNodeData"] ["S.Set", "NodeId"]
    "managerPublicators"

lensInst "sendedTransctions" ["ManagerNodeData"]
    ["BI.Bimap", "TimeSpec", "Transaction"] "managerSendedTransctions"


makeNode :: Chan MsgToSender -> HostAddress -> PortNumber -> Node
makeNode aChan aHostAdress aPortNumber = Node {
        _status         = Noactive
    ,   _mKey           = Nothing
    ,   _chan           = aChan
    ,   _nodePosition   = Nothing
    ,   _nodePort       = aPortNumber
    ,   _isBroadcast    = False
    ,   _nodeHost       = aHostAdress
  }


defaultServerPort :: PortNumber
defaultServerPort = 3000

--
--
instance DistanceTo Node Point where
    distanceTo aNode aPoint = if
        | Just aPosition <- aNode^.nodePosition ->
            distanceTo aPosition  (NodePosition aPoint)
        | otherwise                             -> maxBound

instance DistanceTo Node PointTo where
    distanceTo aNode aPoint = distanceTo aNode (toPoint aPoint)
