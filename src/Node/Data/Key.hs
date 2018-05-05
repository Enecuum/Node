module Node.Data.Key (
        StringKey(..)
    ,   getStringKey
    ,   curve_256
  ) where


import            Crypto.PubKey.ECC.DH
import            Crypto.PubKey.ECC.Types (
    getCurveByName,
    CurveName(SEC_p256k1),
    Curve(..)
  )
import qualified    Data.ByteString                 as B
import qualified    Data.ByteArray                  as BA

newtype StringKey  = StringKey B.ByteString deriving (Eq, Show)

curve_256 :: Curve
curve_256 = getCurveByName SEC_p256k1

getStringKey :: PrivateNumber -> PublicPoint -> StringKey
getStringKey priv pub = StringKey key
  where
    SharedKey sharedKey = getShared curve_256 priv pub
    key = (B.pack . BA.unpack $ sharedKey) :: B.ByteString

--------------------------------------------------------------------------------
