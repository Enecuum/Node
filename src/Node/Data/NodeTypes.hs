{-# LANGUAGE GADTs, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Node.Data.NodeTypes where

import              Service.Network.Base (HostAddress, PortNumber)
import              GHC.Generics
import              Data.Serialize
import              Data.Word
import              Service.Types.PublicPrivateKeyPair (
    uncompressPublicKey,
    getPublicKey,
    compressPublicKey,
    PublicKey(..)
  )
import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA


data NodeVariantRole where
    BroadcastNode   :: NodeVariantRole
    SimpleNode      :: NodeVariantRole
    BootNode        :: NodeVariantRole
    PublicatorNode  :: NodeVariantRole
  deriving (Show, Eq, Ord, Generic)

type NodeVariantRoles = [NodeVariantRole]

instance Serialize NodeVariantRole


newtype NodeId     = NodeId     Integer deriving (Eq, Ord, Num, Enum, Show, Read, Serialize, Real, Integral)
newtype MyNodeId   = MyNodeId   Integer deriving (Eq, Ord, Num, Enum, Show, Read, Serialize, Real, Integral)
newtype ClientId   = ClientId   Word64  deriving (Eq, Ord, Num, Enum, Show, Read, Serialize, Real, Integral)

type BootNodeList   = [(NodeId, HostAddress, PortNumber)]


instance Serialize PortNumber where
    get = toEnum.fromEnum <$> getWord32be
    put aPortNumber = put (toEnum.fromEnum $ aPortNumber :: Word32)

--
toNodeId :: MyNodeId -> NodeId
toNodeId (MyNodeId aId) = NodeId aId


toMyNodeId :: NodeId -> MyNodeId
toMyNodeId (NodeId aId) = MyNodeId aId


keyToId :: ECDSA.PublicKey -> NodeId
keyToId key = case compressPublicKey key of
    PublicKey256k1 a -> NodeId $ toInteger a


idToKey :: NodeId -> ECDSA.PublicKey
idToKey (NodeId aId) = getPublicKey . uncompressPublicKey $ PublicKey256k1 $ fromInteger aId



--
