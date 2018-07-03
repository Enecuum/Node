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
import qualified    Data.ByteString                 as B
import              Data.Serialize
import qualified    Data.Map                        as M
import              GHC.Generics (Generic)
import qualified    Control.Concurrent.Chan         as C
import              Control.Concurrent.Chan.Unagi.Bounded

import              Service.Network.Base
import              Crypto.Random.Types
import              Crypto.PubKey.ECC.ECDSA         as ECDSA
import              Lens.Micro
import              Lens.Micro.TH

import              Node.Crypto
import              Node.Data.Key
import              Node.Data.NetPackage
import qualified    Sharding.Types.Node as N
import              Service.Types (Transaction, Microblock)

import              Data.Scientific (toRealFloat, Scientific)
import              Data.Aeson
import              Data.Aeson.TH
import              Service.InfoMsg
import              PoA.Types
import              Node.FileDB.FileServer


instance Show (InChan a) where
    show _ = "InChan"


type Transactions = [Transaction]

data ExitMsg where
    ExitMsg :: ExitMsg

data MsgToCentralActor where
    ClientIsDisconnected    :: NodeId                   -> MsgToCentralActor
    MakeConnect             :: Connect                  -> MsgToCentralActor
    MsgFromNode             :: MsgToMainActorFromPP     -> MsgToCentralActor
    MsgFromSharding         :: N.ShardingNodeRequestMsg -> MsgToCentralActor
    CleanAction             :: MsgToCentralActor
    NewTransaction          :: Transaction              -> MsgToCentralActor


data MsgToMainActorFromPP
    = MicroblockFromPP Microblock PPId
    | BroadcastRequestFromPP B.ByteString IdFrom NodeType
    | NewConnectWithPP PPId NodeType (InChan NNToPPMessage)
    | MsgResendingToPP IdFrom IdTo B.ByteString
    | PoWListRequest IdFrom
  deriving (Show)

data MsgToServer where
    KillMsg       :: MsgToServer


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
    ,   _bootConnects       :: [Connect]
    ,   _shardingChan       :: Maybe (C.Chan N.ShardingNodeAction)
    ,   _logChan            :: InChan InfoMsg
    ,   _fileServerChan     :: InChan FileActorRequest
    ,   _microblockChan     :: InChan Microblock
    ,   _transactionsChan   :: InChan Transaction
  }
makeLenses ''NetworkNodeData


makeNetworkData
    ::  [Connect]
    ->  NodeConfig
    ->  InChan InfoMsg
    ->  InChan FileActorRequest
    ->  InChan Microblock
    ->  InChan Transaction
    ->  NetworkNodeData
makeNetworkData aBootNodeList aNodeConfig = NetworkNodeData M.empty aNodeConfig aBootNodeList Nothing



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

-- FIXME: find a right place.
makePackageSignature
    ::  Serialize aPackage
    =>  NetworkNodeData
    ->  aPackage
    ->  IO PackageSignature
makePackageSignature aData aResponse = do
    aTime <- getTime Realtime
    let aNodeId = aData^.nodeConfig.myNodeId
    aResponseSignature <- signEncodeble
        (aData^.nodeConfig.privateKey)
        (aNodeId, aTime, aResponse)
    return $ PackageSignature aNodeId aTime aResponseSignature



defaultServerPort :: PortNumber
defaultServerPort = 3000
