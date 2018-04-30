{-# LANGUAGE
        GADTs
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   TemplateHaskell
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Data.NodeTypes where

import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA
import              GHC.Generics
import              Data.Serialize
import              Data.Word
import              Data.Aeson.TH

import              Service.Network.Base (HostAddress, PortNumber)
import              Service.Types.PublicPrivateKeyPair (
        uncompressPublicKey
    ,   getPublicKey
    ,   compressPublicKey
    ,   PublicKey(..)
  )


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

type IdIpPort = (NodeId, HostAddress, PortNumber)
type BootNodeList   = [IdIpPort]

$(deriveJSON defaultOptions ''NodeId)
$(deriveJSON defaultOptions ''MyNodeId)
$(deriveJSON defaultOptions ''ClientId)

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
