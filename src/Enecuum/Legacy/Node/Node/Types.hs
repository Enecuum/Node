{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module provides types for storing internal state of a node and messages.
-- Different nodes use mutually overlapping set of messages
module Enecuum.Legacy.Node.Node.Types where

import qualified Control.Concurrent.Chan               as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import qualified Data.Map                              as M
import           Data.Serialize                        (Serialize)
import qualified Data.Serialize                        as S (get, put)
import           GHC.Generics                          (Generic)
import           Universum

import           Crypto.PubKey.ECC.ECDSA               as ECDSA
import           Crypto.Random.Types
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Sync.SyncJson
import           Lens.Micro.TH

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Scientific                       (Scientific, toRealFloat)
import           Enecuum.Legacy.Node.Data.Key
import           Enecuum.Legacy.Node.DataActor
import           Enecuum.Legacy.Node.NetLvl.Messages
import           Enecuum.Legacy.Service.Types          (InfoMsg, Microblock,
                                                        Transaction)
import qualified Enecuum.Legacy.Sharding.Types.Node    as N
import           Prelude                               (show)

instance Show (InChan a) where show _ = "InChan"
instance Show (MVar a) where show _ = "MVar"


type Transactions = [Transaction]

data ExitMsg where ExitMsg :: ExitMsg

data MsgToCentralActor where
    NodeIsDisconnected      :: NodeId                   -> MsgToCentralActor
    ActionFromNode          ::              MsgFromNode -> MsgToCentralActor
    MsgFromNode             :: NodeType -> NetMessage   -> MsgToCentralActor
    MsgFromSharding         :: N.ShardingNodeRequestMsg -> MsgToCentralActor
    CleanAction             :: MsgToCentralActor
    NewTransaction          :: Transaction -> MVar Bool -> MsgToCentralActor
    SendMsgToNode           :: Value -> IdTo            -> MsgToCentralActor
    SendSyncMsg             :: NodeId -> Value          -> MsgToCentralActor
    SyncToNode              :: SyncMessage  -> NodeId       -> MsgToCentralActor
    ActualConnectsToNNRequest:: MVar [ActualConnectInfo] -> MsgToCentralActor


data MsgFromNode
    = RequestListOfPoW IdFrom
    | RequestActualConnectList (MVar [ActualConnectInfo])
    | NewConnect NodeId NodeType (InChan NetMessage) (Maybe Connect)
  deriving (Show)


-- | TODO: shoud be refactord: reduce keys count.
data NodeConfig = NodeConfig {
        _privateKey :: PrivateKey
    ,   _myNodeId   :: MyNodeId
  }
  deriving (Generic)

deriveJSON defaultOptions ''NodeConfig
makeLenses ''NodeConfig


data NodeInfo = NodeInfo {
        _nodeChan    :: InChan NetMessage
    ,   _nodeType    :: NodeType
    ,   _connectInfo :: Maybe Connect
  }
  deriving (Eq)
makeLenses ''NodeInfo


data NetworkNodeData = NetworkNodeData {
        _connects         :: M.Map NodeId NodeInfo
    ,   _nodeConfig       :: NodeConfig
    ,   _shardingChan     :: Maybe (C.Chan N.ShardingNodeAction)
    ,   _logChan          :: InChan InfoMsg
    ,   _fileServerChan   :: InChan (DataActorRequest Connect)
    ,   _microblockChan   :: InChan Microblock
    ,   _transactionsChan :: InChan (Transaction, MVar Bool)
    ,   _valueChan        :: InChan Value
  }

makeLenses ''NetworkNodeData

makeNetworkData
    ::  NodeConfig
    ->  InChan InfoMsg
    ->  InChan (DataActorRequest Connect)
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
    get = PrivateKey <$> S.get <*> S.get
    put (PrivateKey a b)= S.put a >> S.put b

makeNewNodeConfig :: MonadRandom m => m NodeConfig
makeNewNodeConfig = do
    (aPublicKey,     aPrivateKey)  <- generateKeyPair
    let aId = keyToId aPublicKey
    return $ NodeConfig aPrivateKey (toMyNodeId aId)



defaultServerPort :: PortNumber
defaultServerPort = 3000
