{-# LANGUAGE TemplateHaskell, GADTs, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Node.Data.NetMesseges where

import              GHC.Generics
import              Data.Serialize
import              Data.Word
import              Lens.Micro

import              Service.Network.Base (HostAddress, PortNumber)
import              Node.Template.Constructor
import              Node.Data.NodeTypes
import              Node.Data.Lens


newtype P2pVersion = P2pVersion Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype CapVersion = CapVersion Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)
newtype ListenPort = ListenPort Word64  deriving (Eq, Ord, Num, Enum, Show, Serialize, Real, Integral)

----------
