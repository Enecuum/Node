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

data PackagedMsg where
    ConnectingMsg       ::
        PublicPoint ->
        NodeId ->
        ECDSA.PublicKey ->
        Signature ->
        PackagedMsg
    PackagedMsg         :: PackagedMsgStringEncoded -> PackagedMsg
  deriving (Eq, Generic, Show)


data Package where
    Hello           :: HelloMsg          -> Package
    Disconnect      :: [Reason]          -> Package
    Ping            :: PingPackage       -> Package
    Pong            :: PongPackage       -> Package
    InfoPing        :: InfoPingPackage   -> Package
  deriving (Eq, Generic, Show)


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


data PingPackage where
    EmptyPing   :: PingPackage
    BroadcastNodeListRequest :: PingPackage
    IPRequest   :: TimeSpec -> Signature -> PingPackage
  deriving (Eq, Generic, Show)


data PongPackage where
    EmptyPong               :: PongPackage
    IPAnswer                :: HostAddress -> TimeSpec -> Signature -> PongPackage
    BroadcastNodeListAnswer :: [(NodeId, (HostAddress, PortNumber))] -> PongPackage
  deriving (Eq, Generic, Show)

data InfoPingPackage where
    BlockMade               :: Microblock               -> InfoPingPackage
    NewTransactionInNet     :: Transaction              -> InfoPingPackage
    NewTargetedTransaction  :: Transaction -> NodeId    -> InfoPingPackage
    RoamTransaction         :: Transaction              -> InfoPingPackage
    InfoPingRawPackage      :: B.ByteString               -> InfoPingPackage
    TransactionConfirmation ::
        Transaction
        -> NodeId
        -> Signature
        -> InfoPingPackage
    IAmPublicator           ::
        TimeSpec
        -> NodeId
        -> Signature
        -> InfoPingPackage
    IHaveBroadcastConnects ::
        TimeSpec ->
        Int ->
        HostAddress ->
        PortNumber ->
        NodeId ->
        Signature ->
        InfoPingPackage
  deriving (Generic, Eq, Show)


instance Serialize Package
instance Serialize PackagedMsg
instance Serialize PongPackage
instance Serialize InfoPingPackage
instance Serialize PingPackage
instance Serialize Reason

class IsByteString a where
    toByteString    :: a -> B.ByteString
    fromByteString  :: B.ByteString -> a


encodePackage :: (Serialize a, IsByteString b) => a -> b
encodePackage = fromByteString . encode

decodePackage :: (Serialize a, IsByteString b) => b -> Either String a
decodePackage = decode . toByteString


newtype PackagedMsgStringEncoded = PackagedMsgStringEncoded B.ByteString
    deriving (Eq, Ord, Show, Serialize)

instance IsByteString PackagedMsgStringEncoded where
    fromByteString a = PackagedMsgStringEncoded a
    toByteString (PackagedMsgStringEncoded a) = a

instance Serialize TimeSpec where
    put (TimeSpec a b) = put a *> put b
    get = TimeSpec <$> get <*> get






--------
