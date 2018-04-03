{-# LANGUAGE GADTs, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Node.Data.NetPackage where

import Node.Data.NodeTypes

import              Service.Network.Base (HostAddress, PortNumber)
import              Data.Serialize
import              Data.ByteString as B
import              Service.Types (Transaction, Microblock(..))
import              GHC.Generics
import              System.Clock
import              Crypto.PubKey.ECC.ECDSA (Signature(..))
import              Crypto.PubKey.ECC.DH
import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA
import              Node.Data.NetMesseges
import              Data.Word
import              Sharding.Types.ShardTypes
import              Sharding.Space.Point as P
import              Sharding.Space.Distance

-- | Data for resending from NetNode A to NetNode B.
data Package where
    Ciphered   :: CipheredString -> Package
    Unciphered :: Unciphered     -> Package
  deriving (Eq, Generic, Show)


-- | Unciphered data from NetNode A to NetNode B.
data Unciphered where
    ConnectingRequest  :: PublicPoint -> MyNodeId -> Signature  -> Unciphered
    DisconnectRequest  :: [Reason]                              -> Unciphered
    PingRequest        :: Unciphered
    PongResponce       :: HostAddress -> Unciphered
  deriving (Eq, Generic, Show)


-- | Ciphered  data from NetNode A to NetNode B.
data Ciphered where
    PackageTraceRoutingRequest  :: TraceRouting         -> RequestPackage    -> Ciphered
    PackageTraceRoutingResponce :: TraceRouting         -> ResponcePackage   -> Ciphered
    BroadcastRequest            :: PackageSignature     -> BroadcastThing   -> Ciphered
  deriving (Eq, Generic, Show)


-- | Request data from NetNode A to NetNode B.
data RequestPackage where
    RequestLogicLvlPackage  :: RequestLogicLvl  -> PackageSignature -> RequestPackage
    RequestNetLvlPackage    :: RequestNetLvl    -> PackageSignature -> RequestPackage
  deriving (Eq, Generic, Show)


-- | Request logic information
data RequestLogicLvl where
    ShardIndexRequestPackage    :: P.PointFrom -> Distance P.Point  -> RequestLogicLvl
    ShardRequestPackage         :: ShardHash                        -> RequestLogicLvl
    NodePositionRequestPackage  ::                                     RequestLogicLvl
  deriving (Eq, Generic, Show)


-- | Request network information.
data RequestNetLvl where
    BroadcastListRequest    :: RequestNetLvl
    HostAdressRequest       :: RequestNetLvl
    IsYouBrodcast           :: RequestNetLvl
  deriving (Eq, Generic, Show)


data ResponcePackage where
    ResponceNetLvlPackage   :: ResponceNetLvl   -> PackageSignature -> ResponcePackage
    ResponceLogicLvlPackage :: ResponceLogicLvl -> PackageSignature -> ResponcePackage
  deriving (Eq, Generic, Show)


data ResponceNetLvl where
    BroadcastListResponce   :: [(NodeId, HostAddress, PortNumber)] -> ResponceNetLvl
    HostAdressResponce      :: HostAddress -> ResponceNetLvl
    IAmBroadcast            :: Bool -> ResponceNetLvl
  deriving (Eq, Generic, Show)


data ResponceLogicLvl where
    ShardIndexResponce            :: [ShardHash]    -> ResponceLogicLvl
    ShardResponce                 :: Shard          -> ResponceLogicLvl
    NodePositionResponcePackage   :: MyNodePosition -> ResponceLogicLvl
  deriving (Eq, Generic, Show)


data PackageSignature where
    PackageSignature :: MyNodeId -> TimeSpec  -> Signature  -> PackageSignature
  deriving (Eq, Ord, Show, Generic)


data TraceRouting where
      ToNode     :: NodeId  ->             PackageSignature    -> TraceRouting
      ToDirect   :: P.PointFrom -> P.PointTo -> [PackageSignature] -> TraceRouting
  deriving (Eq, Ord, Show, Generic)


newtype CipheredString = CipheredString B.ByteString
    deriving (Eq, Ord, Show, Serialize)


data BroadcastThing where
    BroadcastWarning      :: BroadcastWarning                   -> BroadcastThing
    BroadcastShard        :: Shard                              -> BroadcastThing
--  BroadcastBlock        :: Block                              -> BroadcastThing
    BroadcastTransaction  :: Transaction                        -> BroadcastThing
    BroadcastPosition     :: MyNodeId           -> NodePosition -> BroadcastThing
  deriving (Eq, Ord, Show, Generic)

data BroadcastWarning = INeedNeighbors MyNodeId HostAddress
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
instance Serialize BroadcastWarning

instance Serialize ResponceNetLvl
instance Serialize ResponceLogicLvl
instance Serialize RequestNetLvl
instance Serialize RequestLogicLvl

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
