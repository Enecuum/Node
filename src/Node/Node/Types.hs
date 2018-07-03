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

import              System.Clock
import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import qualified    Data.Bimap                      as BI
import              Data.Serialize
import              Data.Monoid
import qualified    Data.Map                        as M
import qualified    Crypto.PubKey.ECC.DH            as DH
import              GHC.Generics (Generic)
import qualified    Control.Concurrent.Chan         as C
import              Control.Concurrent.Chan.Unagi.Bounded

import              Crypto.Random.Types
import              Crypto.PubKey.ECC.ECDSA         as ECDSA
import              Lens.Micro
import              Lens.Micro.TH

import              Node.Crypto
import              Node.Data.Key
import              Node.Data.NetPackage
import              Node.Template.Constructor
import              Sharding.Space.Point
import qualified    Sharding.Types.Node as N
import              Service.Types (Transaction, Microblock)
import              Sharding.Space.Distance

import              Data.Scientific (toRealFloat, Scientific)
import              Data.Aeson
import              Data.Aeson.TH
import              Service.InfoMsg
import              Service.Network.Base (ConnectInfo, HostAddress, PortNumber, Connect)
import              PoA.Types
import              Node.FileDB.FileServer


data NodeVariantRole where
    BroadcastNode   :: NodeVariantRole
    SimpleNode      :: NodeVariantRole
    BootNode        :: NodeVariantRole
    PublicatorNode  :: NodeVariantRole
  deriving (Show, Eq, Ord, Generic)

type NodeVariantRoles = [NodeVariantRole]

instance Show (InChan a) where
    show _ = "InChan"
instance Serialize NodeVariantRole

type BootNodeList   = [(NodeId, Connect)]




data Msg where Msg :: B.ByteString -> Msg
type Transactions = [Transaction]

--
idLens :: Lens' a a
idLens = lens Prelude.id (\_ a -> a)

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

data MsgToMainActorFromPP
    = MicroblockFromPP Microblock PPId
    | BroadcastRequestFromPP B.ByteString IdFrom NodeType
    | NewConnectWithPP PPId NodeType (InChan NNToPPMessage)
    | MsgResendingToPP IdFrom IdTo B.ByteString
    | PoWListRequest IdFrom
  deriving (Show)

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


type ShardingChan = C.Chan N.ShardingNodeAction
type MaybeChan a = Maybe (C.Chan a)

-- | TODO: shoud be refactord: reduce keys count.
data NodeConfig = NodeConfig {
        _privateKey    :: PrivateKey
    ,   _myNodeId      :: MyNodeId
  }
  deriving (Generic)

deriveJSON defaultOptions ''NodeConfig
makeLenses ''NodeConfig


data NodeInfo = NodeInfo {
        _nodeChan :: InChan NNToPPMessage
    ,   _nodeType :: NodeType
  }
makeLenses ''NodeInfo


data NetworkNodeData = NetworkNodeData {
        _connects           :: M.Map NodeId NodeInfo
    ,   _nodeConfig         :: NodeConfig
    ,   _shardingChan       :: MaybeChan N.ShardingNodeAction
    ,   _logChan            :: InChan InfoMsg
    ,   _fileServerChan     :: InChan FileActorRequest
    ,   _microblockChan     :: InChan Microblock
    ,   _transactionsChan   :: InChan Transaction
  }
makeLenses ''NetworkNodeData




makeNodeBaseData
    ::  C.Chan ExitMsg
    ->  BootNodeList
    ->  C.Chan Answer
    ->  C.Chan Microblock
    ->  PortNumber
    ->  InChan InfoMsg
    ->  InChan FileActorRequest
    ->  NodeBaseData
makeNodeBaseData aExitChan aList aAnswerChan aMicroblockChan = NodeBaseData
    aExitChan M.empty M.empty aList aAnswerChan 0 Nothing aMicroblockChan
    Nothing Nothing False



type Token = Integer

data RPCBuildConfig where
     RPCBuildConfig :: {
        rpcPort        :: PortNumber,
        enableIP       :: [String],
        accessToken    :: Maybe Token
  } -> RPCBuildConfig
  deriving (Generic)

data SimpleNodeBuildConfig where
     SimpleNodeBuildConfig :: {
        sharding       :: Bool,
        cliMode        :: String,  -- "off", "rpc" or ""cli
        rpcBuildConfig :: Maybe RPCBuildConfig
  } -> SimpleNodeBuildConfig
  deriving (Generic)

instance ToJSON PortNumber where
  toJSON pn = Number $ fromInteger $ toInteger pn

toDouble :: Scientific -> Double
toDouble = toRealFloat

instance FromJSON PortNumber where
    parseJSON (Number s) = return.toEnum.fromEnum.toDouble $ s
    parseJSON _          = error "i've felt with the portnumber parsing"


deriveJSON defaultOptions ''RPCBuildConfig
deriveJSON defaultOptions ''SimpleNodeBuildConfig

deriveJSON defaultOptions ''ConnectInfo

data BuildConfig where
     BuildConfig :: {
        extConnectPort        :: PortNumber,
        poaPort               :: PortNumber,
        bootNodeList          :: String,
        simpleNodeBuildConfig :: Maybe SimpleNodeBuildConfig,
        statsdBuildConfig     :: ConnectInfo,
        logsBuildConfig       :: ConnectInfo
  } -> BuildConfig
  deriving (Generic)

deriveJSON defaultOptions ''BuildConfig



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
        :: C.Chan Transaction
        -> C.Chan Microblock
        -> C.Chan ExitMsg
        -> C.Chan Answer
        -> InChan InfoMsg
        -> InChan FileActorRequest
        -> BootNodeList
        -> NodeConfig
        -> PortNumber
        ->  a

instance ToManagerData ManagerNodeData where
    toManagerData aTransactionChan aMicroblockChan aExitChan aAnswerChan aInfoChan aFileRequestChan aList aNodeConfig aOutPort = ManagerNodeData
        aNodeConfig (makeNodeBaseData aExitChan aList aAnswerChan aMicroblockChan aOutPort aInfoChan aFileRequestChan)
            aTransactionChan BI.empty BI.empty S.empty BI.empty


makeNewNodeConfig :: MonadRandom m => m NodeConfig
makeNewNodeConfig = do
    (aPublicKey,     aPrivateKey)  <- generateKeyPair
    let aId = keyToId aPublicKey
    return $ NodeConfig aPrivateKey (toMyNodeId aId)

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
    aResponseSignature <- signEncodeble
        (aData^.privateKey)
        (aNodeId, aTime, aResponse)
    return $ PackageSignature aNodeId aTime aResponseSignature



defaultServerPort :: PortNumber
defaultServerPort = 3000
