{-# LANGUAGE
        GADTs
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   TypeFamilies
    ,   FlexibleInstances
#-}

module Node.Data.NetPackage where

import Node.Data.NodeTypes

import              Data.Serialize
import              Data.ByteString as B
import              System.Clock
import              GHC.Generics
import              Crypto.PubKey.ECC.ECDSA (Signature(..))
import              Crypto.PubKey.ECC.DH

import              Service.Network.Base (HostAddress, PortNumber)
import              Service.Types (Transaction, Microblock)

import              Sharding.Types.ShardTypes
import              Sharding.Space.Point as P
import              Sharding.Space.Distance

-- | Data for resending from NetNode A to NetNode B.
data Package where
    Ciphered   :: CipheredString -> Package
    Unciphered :: Unciphered     -> Package
  deriving (Eq, Generic, Show)


-- | Unciphered data from NetNode A to NetNode B.
-- TODO: Add the Node id of addresat and it verification for multy nodes on one ip.
data Unciphered where
    ConnectingRequest
        ::  PublicPoint
        ->  MyNodeId
        ->  PortNumber
        ->  Signature
        ->  Unciphered

    DisconnectRequest  :: [Reason]                              ->  Unciphered
    PingRequest        ::                                           Unciphered
    PongResponce       :: HostAddress                           ->  Unciphered
  deriving (Eq, Generic, Show)


-- | Ciphered  data from NetNode A to NetNode B.
data Ciphered where
    PackageTraceRoutingRequest
        ::  TraceRouting
        ->  RequestPackage
        ->  Ciphered

    PackageTraceRoutingResponce
        ::  TraceRouting
        ->  ResponcePackage
        ->  Ciphered

    BroadcastRequest
        ::  PackageSignature
        ->  BroadcastThing
        ->  Ciphered
  deriving (Eq, Generic, Show)


-- | Request data from NetNode A to NetNode B.
data RequestPackage where
    RequestLogicLvlPackage
        ::  Request LogicLvl
        ->  PackageSignature
        ->  RequestPackage

    RequestNetLvlPackage
        ::  Request NetLvl
        ->  PackageSignature
        ->  RequestPackage
  deriving (Eq, Generic, Show)


data ResponcePackage where
    ResponceNetLvlPackage
        ::  RequestPackage
        ->  Responce NetLvl
        ->  PackageSignature
        ->  ResponcePackage

    ResponceLogicLvlPackage
        ::  RequestPackage
        ->  Responce LogicLvl
        ->  PackageSignature
        ->  ResponcePackage
  deriving (Eq, Generic, Show)


data LogicLvl   = LogicLvl  deriving Show
data NetLvl     = NetLvl    deriving Show
data MiningLvl  = MiningLvl deriving Show

data family Request a :: *


--data _____ = _____Request NodeId MyNodeId Signature


-- | Request logic information
data instance Request LogicLvl where
    ShardIndexRequestPackage
        ::  P.PointFrom
        ->  Distance P.Point
        ->  Request LogicLvl
    NeighborListRequestPackage      :: Request LogicLvl
    ShardRequestPackage             :: ShardHash -> Request LogicLvl
    NodePositionRequestPackage      :: Request LogicLvl
    IsAliveTheNodeRequestPackage    :: NodeId -> Request LogicLvl
  deriving (Eq, Generic, Show)


-- | Request network information.
data instance Request NetLvl where
    BroadcastListRequest    :: Request NetLvl
    --  TODO: Make a sending of HostAdressRequest.
    HostAdressRequest       :: Request NetLvl
    --  TODO: Make a sending of IsYouBrodcast.
    IsYouBrodcast           :: Request NetLvl
  deriving (Eq, Generic, Show)


data family Responce a :: *

data instance Responce NetLvl where
    BroadcastListResponce
        ::  NodeInfoList LogicLvl
        ->  NodeInfoList NetLvl
        ->  Responce NetLvl

    HostAdressResponce
        ::  Maybe HostAddress
        ->  Responce NetLvl

    IAmBroadcast
        ::  Bool
        ->  Responce NetLvl

  deriving (Eq, Generic, Show)

data family NodeInfoList a :: *

data instance NodeInfoList LogicLvl where
    NodeInfoListLogicLvl
        ::  [(NodeId, NodePosition)]
        ->  NodeInfoList LogicLvl
  deriving (Eq, Generic, Show)


data instance NodeInfoList NetLvl where
    NodeInfoListNetLvl
        ::  [(NodeId, HostAddress, PortNumber)]
        ->  NodeInfoList NetLvl
  deriving (Eq, Generic, Show)


data instance Responce LogicLvl where
    ShardIndexResponce            :: [ShardHash]    -> Responce LogicLvl
    ShardResponce                 :: [Shard]        -> Responce LogicLvl
    NodePositionResponcePackage   :: MyNodePosition -> Responce LogicLvl
    NeighborListResponcePackage   :: [(NodeId, NodePosition)] -> Responce LogicLvl
    TheNodeIsAlive                :: NodeId -> Bool -> Responce LogicLvl
  deriving (Eq, Generic, Show)


data PackageSignature where
    PackageSignature :: MyNodeId -> TimeSpec  -> Signature  -> PackageSignature
  deriving (Eq, Ord, Show, Generic)


data TraceRouting where
    ToNode   :: NodeId -> PackageSignature -> TraceRouting
    ToDirect :: P.PointFrom -> P.PointTo -> [PackageSignature] -> TraceRouting
  deriving (Eq, Ord, Show, Generic)


newtype CipheredString = CipheredString B.ByteString
    deriving (Eq, Ord, Show, Serialize)


data BroadcastThing where
    BroadcastNet          :: BroadcastThingLvl NetLvl       -> BroadcastThing
    BroadcastLogic        :: BroadcastThingLvl LogicLvl     -> BroadcastThing
    BroadcastMining       :: BroadcastThingLvl MiningLvl    -> BroadcastThing
  deriving (Eq, Ord, Show, Generic)


data family BroadcastThingLvl a

data instance BroadcastThingLvl LogicLvl where
    BroadcastShard      :: Shard    -> BroadcastThingLvl LogicLvl
    BroadcastPosition   :: MyNodeId -> NodePosition -> BroadcastThingLvl LogicLvl
  deriving (Eq, Ord, Show, Generic)



data instance BroadcastThingLvl MiningLvl where
    BroadcastTransaction
        ::  Transaction
        ->  Maybe NodeId
        ->  BroadcastThingLvl MiningLvl

    BroadcastMicroBlock
        ::  Microblock
        ->  Maybe NodeId
        ->  BroadcastThingLvl MiningLvl

    BroadcastBlockIndex
        ::  B.ByteString
        ->  Maybe NodeId
        ->  BroadcastThingLvl MiningLvl

    BroadcastKeyBlock
        ::  B.ByteString
        ->  Maybe NodeId
        ->  BroadcastThingLvl MiningLvl
  deriving (Eq, Ord, Show, Generic)


data instance BroadcastThingLvl NetLvl where
    INeedNeighbors
        ::  MyNodeId
        ->  HostAddress
        ->  PortNumber
        ->  BroadcastThingLvl NetLvl
  deriving (Eq, Ord, Show, Generic)


data Reason where
    DisconnectRequsted          :: Reason
    TcpSubSystemError           :: Reason
    BreachOfProtocol            :: Reason
    UselessPeer                 :: Reason
    TooManyPeers                :: Reason
    AlreadyConnected            :: Reason
    IncompatibleProtocolVersion :: Reason
    NullNodeIdentityReceived    :: Reason
    ClientQuitting              :: Reason
    UnexpectedIdentity          :: Reason
    IdentityIsTheSameAsThisNode :: Reason
    TimeoutOnReceivingAMessage  :: Reason
    SomeOtherReason             :: Reason
  deriving (Generic, Eq, Enum, Show)


--------------------------------------------------------------------------------
instance Serialize Reason
instance Serialize BroadcastThing
instance Serialize TraceRouting
instance Serialize PackageSignature
instance Serialize ResponcePackage
instance Serialize Ciphered
instance Serialize Package
instance Serialize Unciphered
instance Serialize RequestPackage

instance Serialize (Responce NetLvl)
instance Serialize (Responce LogicLvl)
instance Serialize (Request  NetLvl)
instance Serialize (Request  LogicLvl)

instance Serialize (NodeInfoList NetLvl)
instance Serialize (NodeInfoList LogicLvl)

instance Serialize (BroadcastThingLvl LogicLvl)
instance Serialize (BroadcastThingLvl MiningLvl)
instance Serialize (BroadcastThingLvl NetLvl)


--------------------------------------------------------------------------------
class IsByteString a where
      toByteString    :: a -> B.ByteString
      fromByteString  :: B.ByteString -> a


encodePackage :: (Serialize a, IsByteString b) => a -> b
encodePackage = fromByteString . encode

decodePackage :: (Serialize a, IsByteString b) => b -> Either String a
decodePackage = decode . toByteString


instance IsByteString CipheredString where
    fromByteString a = CipheredString a
    toByteString (CipheredString a) = a


instance Serialize TimeSpec where
    put (TimeSpec a b) = put a *> put b
    get = TimeSpec <$> get <*> get

--------
-- 1.
-- A, B -- don't have white ip.
-- C, D -- is a broadcast nodes.

-- 1.1. A or B is dead.
-- 1.2. C or D is dead.

-- 2
-- A, B -- is a broadcast nodes.

-- 2.1. A or B is dead.


-- IDEA: About node is dead.
-- Time a live of msg 5-6 hops.
    -- 1. Send msg "DISCONNECT".
    -- 2. Send msg "I AM ALIVE".
-- 3. Aggregation information of alive nodes and send to another





--------------------------------------------------------------------------------
