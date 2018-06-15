{-# LANGUAGE
        GADTs
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   TypeFamilies
    ,   FlexibleInstances
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Data.NetPackage where

import              Data.Serialize
import              Data.ByteString as B
import              System.Clock
import              GHC.Generics
import              Crypto.PubKey.ECC.ECDSA (Signature(..))
import              Crypto.PubKey.ECC.DH

import              Service.Network.Base
import              Service.Types (Transaction, MicroblockV1)

import              Sharding.Types.ShardTypes
import              Sharding.Space.Point as P
import              Sharding.Space.Distance
import              PoA.Types
import              Node.Data.Key

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
    PongResponse       :: HostAddress                           ->  Unciphered
  deriving (Eq, Generic, Show)


-- | Ciphered  data from NetNode A to NetNode B.
data Ciphered where
    PackageTraceRoutingRequest
        ::  TraceRouting
        ->  RequestPackage
        ->  Ciphered

    PackageTraceRoutingResponse
        ::  TraceRouting
        ->  ResponsePackage
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

    RequestMiningLvlPackage
        ::  Request MiningLvl
        ->  PackageSignature
        ->  RequestPackage
  deriving (Eq, Generic, Show)


data ResponsePackage where
    ResponseNetLvlPackage
        ::  RequestPackage
        ->  Response NetLvl
        ->  PackageSignature
        ->  ResponsePackage

    ResponseLogicLvlPackage
        ::  RequestPackage
        ->  Response LogicLvl
        ->  PackageSignature
        ->  ResponsePackage

    ResponseMiningLvlPackage
        :: RequestPackage
        -> Response MiningLvl
        -> PackageSignature
        -> ResponsePackage

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
    -- XXX: TODO: Make a handle of the neighbours list request and response.
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


data instance Request MiningLvl where
    PPMessage
        ::  B.ByteString
        ->  IdFrom
        ->  IdTo
        ->  Request MiningLvl
    RequestPPConnection :: PPId -> Request MiningLvl
  deriving (Eq, Show, Generic)

data family Response a :: *

--
data instance Response MiningLvl where
    ResponsePPConnection :: PPId -> Connect -> Response MiningLvl
  deriving (Eq, Show, Generic)


data instance Response NetLvl where
    BroadcastListResponse
        ::  NodeInfoList LogicLvl
        ->  NodeInfoList NetLvl
        ->  Bool
        ->  Response NetLvl

    HostAdressResponse
        ::  Maybe HostAddress
        ->  Response NetLvl

    IAmBroadcast
        ::  Bool
        ->  Response NetLvl

  deriving (Eq, Generic, Show)

data family NodeInfoList a :: *

data instance NodeInfoList LogicLvl where
    NodeInfoListLogicLvl
        ::  [(NodeId, NodePosition)]
        ->  NodeInfoList LogicLvl
  deriving (Eq, Generic, Show)


data instance NodeInfoList NetLvl where
    NodeInfoListNetLvl
        ::  [(NodeId, Connect)]
        ->  NodeInfoList NetLvl
  deriving (Eq, Generic, Show)


data instance Response LogicLvl where
    ShardIndexResponse            :: [ShardHash]    -> Response LogicLvl
    ShardResponse                 :: [Shard]        -> Response LogicLvl
    NodePositionResponsePackage   :: MyNodePosition -> Response LogicLvl
    NeighborListResponsePackage   :: [(NodeId, NodePosition)] -> Response LogicLvl
    TheNodeIsAlive                :: NodeId -> Bool -> Response LogicLvl
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
    BroadcastPPMsg
        :: NodeType
        -> B.ByteString
        -> NodeType
        -> IdFrom
        -> BroadcastThingLvl MiningLvl

    BroadcastTransaction
        ::  Transaction
        ->  Maybe NodeId
        ->  BroadcastThingLvl MiningLvl

    BroadcastMicroBlock
        ::  MicroblockV1
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
instance Serialize ResponsePackage
instance Serialize Ciphered
instance Serialize Package
instance Serialize Unciphered
instance Serialize RequestPackage

instance Serialize (Response NetLvl)
instance Serialize (Response LogicLvl)
instance Serialize (Response MiningLvl)

instance Serialize (Request  NetLvl)
instance Serialize (Request  LogicLvl)
instance Serialize (Request  MiningLvl)

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
    fromByteString = CipheredString
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
