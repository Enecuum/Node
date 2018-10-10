{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Enecuum.Legacy.Node.Data.Key (
        PublicKey(..)
    ,   NodeId(..)
    ,   MyNodeId(..)
    ,   IdFrom(..)
    ,   IdTo(..)
    ,   curve_256
    ,   toNodeId
    ,   toMyNodeId
    ,   keyToId
    ,   generateKeyPair
    ,   generateClientId
  ) where

import qualified Crypto.PubKey.ECC.ECDSA                           as ECDSA
import           Crypto.PubKey.ECC.Generate
import           Crypto.PubKey.ECC.Types                           (Curve (..), CurveName (SEC_p256k1),
                                                                    getCurveByName)
import           Crypto.Random.Types                               (MonadRandom (..))
import           Data.Aeson.TH
import           Data.Bits
import           Data.Serialize
import           Data.Word
import           Enecuum.Legacy.Refact.Crypto.PublicPrivateKeyPair (PublicKey (..),
                                                                    compressPublicKey)
import           Enecuum.Prelude
import           System.Random

newtype NodeId     = NodeId     Integer deriving (Eq, Ord, Num, Enum, Show, Read, Serialize, Real, Integral)
newtype MyNodeId   = MyNodeId   Integer deriving (Eq, Ord, Num, Enum, Show, Read, Serialize, Real, Integral)
newtype IdFrom     = IdFrom     NodeId  deriving (Show, Ord, Eq, Generic, Serialize)
newtype IdTo       = IdTo       NodeId  deriving (Show, Ord, Eq, Generic, Serialize)

curve_256 :: Curve
curve_256 = getCurveByName SEC_p256k1

deriveJSON defaultOptions ''NodeId
deriveJSON defaultOptions ''MyNodeId


toNodeId :: MyNodeId -> NodeId
toNodeId (MyNodeId aId) = NodeId aId

toMyNodeId :: NodeId -> MyNodeId
toMyNodeId (NodeId aId) = MyNodeId aId

keyToId :: ECDSA.PublicKey -> NodeId
keyToId key = case compressPublicKey key of
    PublicKey256k1 a -> NodeId $ toInteger a

generateKeyPair :: MonadRandom m => m (ECDSA.PublicKey, ECDSA.PrivateKey)
generateKeyPair = generate curve_256

generateClientId :: [Word64] -> IO NodeId
generateClientId list = do
    aRand <- randomIO :: IO Word64
    pure $ NodeId $ fromIntegral $ mask .|. (shiftL aRand ((length list) * 2))
  where
    bitsmask []       _ = 0
    bitsmask (x : xs) n = (bitsmask xs (n + 1)) .|. (shiftL x (2 * n))

    mask = bitsmask (reverse list) 0
