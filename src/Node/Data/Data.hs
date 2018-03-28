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


data HashMsg where
    HashMsgTransactionsRequest :: Int        -> HashMsg
    MBlock                     :: Microblock -> HashMsg
  deriving (Generic, Show)

instance Serialize HashMsg



--------------------------------------------------------------------------------
-- Keys
newtype StringKey  = StringKey B.ByteString deriving (Eq, Show)


curve :: Curve
curve = getCurveByName SEC_p256k1

-- TODO to getKay
getKay :: PrivateNumber -> PublicPoint -> StringKey
getKay priv pub = StringKey key
  where
    SharedKey sharedKey = getShared curve priv pub
    key = (B.pack . BA.unpack $ sharedKey) :: B.ByteString

--------------------------------------------------------------------------------
