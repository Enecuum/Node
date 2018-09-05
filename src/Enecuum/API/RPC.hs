{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.API.RPC where

import           Enecuum.Prelude

import qualified Enecuum.Framework.Domain as D

newtype FindNodeByTagRequest = FindNodeByTagRequest Text
newtype FindNodeByTagResponse = FindNodeByTagResponse D.NodeAddress
  deriving (Generic, Newtype)

-- TODO: more type safety.
instance D.NetworkMethod () FindNodeByTagRequest FindNodeByTagResponse where
  toNetworkRequest _ (FindNodeByTagRequest _) = D.MulticastRequest "dummy"
  fromNetworkResponse _ (D.NetworkResponse _) = error "fromNetworkResponse not implemented for FindNodeByTagRequest"
