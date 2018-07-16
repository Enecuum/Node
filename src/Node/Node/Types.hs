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

import              Data.Serialize
import qualified    Data.Map                        as M
import              GHC.Generics (Generic)
import qualified    Control.Concurrent.Chan         as C
import              Control.Concurrent.MVar
import              Control.Concurrent.Chan.Unagi.Bounded

import              Service.Network.Base
import              Crypto.Random.Types
import              Crypto.PubKey.ECC.ECDSA         as ECDSA
import              Lens.Micro.TH

import              Node.Data.Key
import qualified    Sharding.Types.Node as N
import              Service.Types (Transaction, Microblock)

import              Data.Scientific (toRealFloat, Scientific)
import              Data.Aeson
import              Data.Aeson.TH
import              Service.InfoMsg
import              PoA.Types
import              Node.FileDB.FileServer


instance Show (InChan a) where show _ = "InChan"
instance Show (MVar a) where show _ = "MVar"

type Transactions = [Transaction]

data ExitMsg where ExitMsg :: ExitMsg

data MsgToCentralActor where
    NodeIsDisconnected      :: NodeId                   -> MsgToCentralActor
    ActionFromNode          :: NodeType -> MsgFromNode -> MsgToCentralActor
    MsgFromNode             :: NodeType -> NetMessage  -> MsgToCentralActor
    MsgFromSharding         :: N.ShardingNodeRequestMsg -> MsgToCentralActor
    CleanAction             :: MsgToCentralActor
    NewTransaction          :: Transaction -> MVar Bool -> MsgToCentralActor


data MsgFromNode
    = AcceptedMicroblock Microblock
    | AcceptedTransaction Transaction
    | AcceptedKeyBlock Value
    | ResendingBroadcast Value IdFrom NodeType
    | ResendingMsgTo IdFrom IdTo Value
    | RequestListOfPoW IdFrom
    | RequestActualConnectList (MVar [ActualConnectInfo])
    | NewConnect NodeId (InChan NetMessage) (Maybe Connect)
  deriving (Show)


-- | TODO: shoud be refactord: reduce keys count.
data NodeConfig = NodeConfig {
        _privateKey    :: PrivateKey
    ,   _myNodeId      :: MyNodeId
  }
  deriving (Generic)

deriveJSON defaultOptions ''NodeConfig
makeLenses ''NodeConfig


data NodeInfo = NodeInfo {
        _nodeChan     :: InChan NetMessage
    ,   _nodeType     :: NodeType
    ,   _connectInfo  :: Maybe Connect
  }
  deriving (Eq)
makeLenses ''NodeInfo


data NetworkNodeData = NetworkNodeData {
        _connects           :: M.Map NodeId NodeInfo
    ,   _nodeConfig         :: NodeConfig
    ,   _shardingChan       :: Maybe (C.Chan N.ShardingNodeAction)
    ,   _logChan            :: InChan InfoMsg
    ,   _fileServerChan     :: InChan FileActorRequest
    ,   _microblockChan     :: InChan Microblock
    ,   _transactionsChan   :: InChan (Transaction, MVar Bool)
    ,   _valueChan          :: InChan Value
  }

makeLenses ''NetworkNodeData

makeNetworkData
    ::  NodeConfig
    ->  InChan InfoMsg
    ->  InChan FileActorRequest
    ->  InChan Microblock
    ->  InChan (Transaction, MVar Bool)
    ->  InChan Value
    ->  NetworkNodeData
makeNetworkData aNodeConfig = NetworkNodeData M.empty aNodeConfig Nothing


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

instance Serialize NodeConfig

instance Serialize PrivateKey where
    get = PrivateKey <$> get <*> get
    put (PrivateKey a b)= put a >> put b

makeNewNodeConfig :: MonadRandom m => m NodeConfig
makeNewNodeConfig = do
    (aPublicKey,     aPrivateKey)  <- generateKeyPair
    let aId = keyToId aPublicKey
    return $ NodeConfig aPrivateKey (toMyNodeId aId)



defaultServerPort :: PortNumber
defaultServerPort = 3000
