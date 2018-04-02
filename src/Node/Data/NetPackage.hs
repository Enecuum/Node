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


data Package where
    Ciphered   :: CipheredString -> Package
    Unciphered :: Unciphered     -> Package
  deriving (Eq, Generic, Show)

data Unciphered where
    ConnectingRequest  :: PublicPoint -> MyNodeId -> Signature  -> Unciphered
    DisconnectRequest  :: [Reason]                              -> Unciphered
    PingRequest        :: Unciphered
    PongResponce       :: HostAddress -> Unciphered
  deriving (Eq, Generic, Show)


data Ciphered where
    PackageTraceRoutingRequest  :: TraceRouting         -> RequestPackage    -> Ciphered
    PackageTraceRoutingResponce :: TraceRouting         -> ResponcePackage   -> Ciphered
    BroadcastRequest            :: BroadcastSignature   -> BroadcastThing    -> Ciphered
  deriving (Eq, Generic, Show)


data RequestPackage where
    ShardIndexRequestPackage    :: P.Point -> Distance P.Point -> RequestPackage
    ShardRequestPackage         :: ShardHash -> RequestPackage
    BroadcastListRequest        :: RequestPackage
  deriving (Eq, Generic, Show)


data ResponcePackage where
    ConfirmResponce         :: RequestPackage                 -> ResponcePackage
    ShardIndexResponce      :: RequestPackage -> [ShardHash]  -> ResponcePackage
    ShardResponce           :: RequestPackage -> Shard        -> ResponcePackage
    BroadcastListResponce   :: RequestPackage -> [(NodeId, HostAddress, PortNumber)] -> ResponcePackage
  deriving (Eq, Generic, Show)


data BroadcastSignature where
    BroadcastSignature :: MyNodeId -> TimeSpec  -> Signature  -> BroadcastSignature
  deriving (Eq, Ord, Show, Generic)

data TraceRouting where
      ToNode     :: MyNodeId -> NodeId ->  TimeSpec  -> Signature  -> TraceRouting
      ToDirect   :: [(NodeId, TimeSpec, Signature)] -> P.Point    -> TraceRouting
  deriving (Eq, Ord, Show, Generic)


newtype CipheredString = CipheredString B.ByteString
    deriving (Eq, Ord, Show, Serialize)


data BroadcastThing where
    BroadcastWarning      :: BroadcastWarning               -> BroadcastThing
    BroadcastShard        :: Shard                          -> BroadcastThing
--  BroadcastBlock        :: Block                          -> BroadcastThing
    BroadcastTransaction  :: Transaction                    -> BroadcastThing
    BroadcastPosition     :: MyNodeId           -> P.Point  -> BroadcastThing
  deriving (Eq, Ord, Show, Generic)

data BroadcastWarning = INeedNeighbors MyNodeId HostAddress BroadcastWarning
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
instance Serialize BroadcastSignature
instance Serialize ResponcePackage
instance Serialize Ciphered
instance Serialize Package
instance Serialize Unciphered
instance Serialize RequestPackage
instance Serialize BroadcastWarning

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
