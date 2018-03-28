{-# LANGUAGE TemplateHaskell, GADTs, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Node.Data.NetMesseges where

import              GHC.Generics
import              Data.Serialize
import              Data.Word
import              Lens.Micro

import              Node.Data.Lens
import              Service.Network.Base (HostAddress, PortNumber)
import              Node.Template.Constructor
import              Node.Data.NodeTypes
---
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


newtype P2pVersion = P2pVersion Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype CapVersion = CapVersion Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype ListenPort = ListenPort Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)


data Cap where
    Eth :: Cap
    Ssh :: Cap
  deriving (Eq, Generic, Show)

type Caps           = [(Cap, CapVersion)]

instance Serialize HelloMsg
instance Serialize Cap

genDataClass        "helloMsg" helloMsgList
genBazeDataInstance "helloMsg" (fst <$> helloMsgList)

----------
