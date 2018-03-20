{-# LANGUAGE
    GADTs,
    GeneralizedNewtypeDeriving,
    DeriveGeneric,
    TemplateHaskell,
    StandaloneDeriving
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Node.Data.Data where

import            Service.Network.Base (HostAddress, PortNumber)
import            GHC.Generics (Generic)
import            Crypto.PubKey.ECC.DH
import            Crypto.PubKey.ECC.ECDSA (Signature(..))
import            Crypto.PubKey.ECC.Types (
    getCurveByName,
    CurveName(SEC_p256k1),
    Curve(..)
  )
import              System.Clock
import              Service.Types.PublicPrivateKeyPair (
    uncompressPublicKey,
    getPublicKey,
    compressPublicKey,
    PublicKey(..)
  )
import              Service.Types (Transaction, Microblock(..))
import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA
import qualified    Data.ByteString                 as B
import qualified    Data.ByteArray                  as BA
import              Data.Serialize
import              Data.Word
import              Lens.Micro
import              Node.Template.Constructor


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

data NodeVariantRole where
    BroadcastNode   :: NodeVariantRole
    SimpleNode      :: NodeVariantRole
    BootNode        :: NodeVariantRole
    PublicatorNode  :: NodeVariantRole
  deriving (Show, Eq, Ord, Generic)



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

deriving instance Eq Microblock

data HelloMsg where
    HelloMsg :: {
        helloMsgP2pVersion          :: P2pVersion,
        helloMsgClientId            :: ClientId,
        helloMsgListenPort          :: PortNumber,
        helloMsgNodeId              :: NodeId,
        helloMsgCaps                :: Caps,
        helloMsgNodeVariantRoles    :: NodeVariantRoles
    } -> HelloMsg
  deriving (Eq, Generic, Show)

type NodeVariantRoles = [NodeVariantRole]

data Cap where
    Eth :: Cap
    Ssh :: Cap
  deriving (Eq, Generic, Show)

data BranchType where
    StatisticalBranch   :: BranchType
    EmissiveBranch      :: BranchType
    DataBranch          :: BranchType
    EcologicalBranch    :: BranchType
    ServiceBranch       :: BranchType
    TokenBranch         :: BranchType
  deriving (Eq, Ord, Generic, Show)

data HashMsg where
    HashMsgTransactionsRequest :: Int        -> HashMsg
    MBlock                     :: Microblock -> HashMsg
  deriving (Generic, Show)

instance Serialize PortNumber where
    get = toEnum.fromEnum <$> getWord32be
    put aPortNumber = put (toEnum.fromEnum $ aPortNumber :: Word32)

instance Serialize HashMsg


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

instance Serialize NodeVariantRole

type Caps           = [(Cap, CapVersion)]
type BootNodeList   = [(NodeId, HostAddress, PortNumber)]

newtype P2pVersion = P2pVersion Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype ClientId   = ClientId   Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype CapVersion = CapVersion Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype ListenPort = ListenPort Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype NodeId     = NodeId     Integer deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype MyNodeId   = MyNodeId   Integer deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype StringKey  = StringKey B.ByteString deriving (Eq, Show)


toNodeId :: MyNodeId -> NodeId
toNodeId (MyNodeId aId) = NodeId aId


toMyNodeId :: NodeId -> MyNodeId
toMyNodeId (NodeId aId) = MyNodeId aId


idLens :: Lens' a a
idLens = lens id (\_ a -> a)


keyToId :: ECDSA.PublicKey -> NodeId
keyToId key = case compressPublicKey key of
    PublicKey256k1 a -> NodeId $ toInteger a


idToKey :: NodeId -> ECDSA.PublicKey
idToKey (NodeId aId) = getPublicKey . uncompressPublicKey $ PublicKey256k1 $ fromInteger aId


curve :: Curve
curve = getCurveByName SEC_p256k1


getKay :: PrivateNumber -> PublicPoint -> StringKey
getKay priv pub = StringKey key
  where
    SharedKey sharedKey = getShared curve priv pub
    key = (B.pack . BA.unpack $ sharedKey) :: B.ByteString


genDataClass        "helloMsg" helloMsgList
genBazeDataInstance "helloMsg" (fst <$> helloMsgList)

instance Serialize PongPackage
instance Serialize InfoPingPackage
instance Serialize PingPackage
instance Serialize BranchType
instance Serialize Package
instance Serialize HelloMsg
instance Serialize Cap
instance Serialize Reason
instance Serialize PackagedMsg

instance Serialize TimeSpec where
    put (TimeSpec a b) = put a *> put b
    get = TimeSpec <$> get <*> get
