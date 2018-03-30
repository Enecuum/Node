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

data PackagedMsg where
    ConnectingMsg :: PublicPoint -> NodeId -> ECDSA.PublicKey -> Signature -> PackagedMsg
    PackagedMsg         :: PackagedMsgStringEncoded -> PackagedMsg
  deriving (Eq, Generic, Show)


data Package where
    Hello                   :: HelloMsg                                 -> Package
    Disconnect              :: [Reason]                                 -> Package
    Ping                    :: PingPackage                              -> Package
    Pong                    :: PongPackage                              -> Package
    InfoPing                :: InfoPingPackage                          -> Package
    Request                 :: [(NodeId, TimeSpec, Signature)] -> RequestPackage  -> Package
    Answer                  :: [(NodeId, TimeSpec, Signature)] -> AnswerPackage                -> Package
    ConfirmationOfRequest   :: [(NodeId, TimeSpec, Signature)] -> ConfirmationOfRequestPackage -> Package
  deriving (Eq, Generic, Show)


data RequestPackage where
    ShardIndexRequestPackage    :: NodeId   -> P.Point -> MyNodeId -> TimeSpec -> Word64    -> Signature  -> RequestPackage
    ShardRequestAdressedPackage :: NodeId   -> P.Point -> MyNodeId -> TimeSpec -> ShardHash -> Signature  -> RequestPackage
    ShardRequestPackage         ::             P.Point -> MyNodeId -> TimeSpec -> ShardHash -> Signature  -> RequestPackage
  deriving (Eq, Generic, Show)

data AnswerPackage where
    ShardIndexAnswerPackage :: NodeId -> MyNodeId -> TimeSpec -> Word64 -> [ShardHash] -> Signature -> AnswerPackage
    ShardAnswerPackage      ::           MyNodeId -> TimeSpec -> ShardHash -> Shard    -> Signature -> AnswerPackage
  deriving (Eq, Generic, Show)


data ConfirmationOfRequestPackage where
    ConfirmationOfRequestPackage :: ConfirmationOfRequestPackage
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
    NewShardInNetMessage    :: Shard -> InfoPingPackage
    TheNodeHavePosition     :: MyNodeId -> P.Point -> TimeSpec -> Signature -> InfoPingPackage
    IamAwakeMessage         :: MyNodeId -> P.Point -> TimeSpec -> Signature -> InfoPingPackage
    -- ?????
    BlockMade               :: Microblock               -> InfoPingPackage
    NewTransactionInNet     :: Transaction              -> InfoPingPackage

    IHaveBroadcastConnects ::
        TimeSpec ->
        Int ->
        HostAddress ->
        PortNumber ->
        NodeId ->
        Signature ->
        InfoPingPackage
  deriving (Generic, Eq, Show)


instance Serialize RequestPackage
instance Serialize AnswerPackage
instance Serialize ConfirmationOfRequestPackage
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
