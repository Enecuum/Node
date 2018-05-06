{-# LANGUAGE
        GADTs
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   TemplateHaskell
  #-}

module Node.Data.Key (
        StringKey(..)
    ,   NodeId(..)
    ,   MyNodeId(..)
    ,   getStringKey
    ,   curve_256
    ,   toNodeId
    ,   toMyNodeId
    ,   keyToId
    ,   idToKey
  ) where


import            Crypto.PubKey.ECC.DH
import            Crypto.PubKey.ECC.Types (
    getCurveByName,
    CurveName(SEC_p256k1),
    Curve(..)
  )
import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA
import qualified    Data.ByteString                 as B
import qualified    Data.ByteArray                  as BA
import              Data.Aeson.TH
import              Data.Serialize
import              Service.Types.PublicPrivateKeyPair (
        uncompressPublicKey
    ,   getPublicKey
    ,   compressPublicKey
    ,   PublicKey(..)
  )

newtype NodeId     = NodeId     Integer deriving (Eq, Ord, Num, Enum, Show, Read, Serialize, Real, Integral)
newtype MyNodeId   = MyNodeId   Integer deriving (Eq, Ord, Num, Enum, Show, Read, Serialize, Real, Integral)

newtype StringKey  = StringKey B.ByteString deriving (Eq, Show)

curve_256 :: Curve
curve_256 = getCurveByName SEC_p256k1

getStringKey :: PrivateNumber -> PublicPoint -> StringKey
getStringKey priv pub = StringKey key
  where
    SharedKey sharedKey = getShared curve_256 priv pub
    key = (B.pack . BA.unpack $ sharedKey) :: B.ByteString


deriveJSON defaultOptions ''NodeId
deriveJSON defaultOptions ''MyNodeId


toNodeId :: MyNodeId -> NodeId
toNodeId (MyNodeId aId) = NodeId aId


toMyNodeId :: NodeId -> MyNodeId
toMyNodeId (NodeId aId) = MyNodeId aId


keyToId :: ECDSA.PublicKey -> NodeId
keyToId key = case compressPublicKey key of
    PublicKey256k1 a -> NodeId $ toInteger a


idToKey :: NodeId -> ECDSA.PublicKey
idToKey (NodeId aId) = getPublicKey . uncompressPublicKey $ PublicKey256k1 $ fromInteger aId


--------------------------------------------------------------------------------
