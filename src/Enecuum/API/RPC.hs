{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.API.RPC where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                      as T
import           Enecuum.Framework.Domain.Node           ( NodeConfig )

newtype FindNodeByTagRequest = FindNodeByTagRequest Text
newtype FindNodeByTagResponse = FindNodeByTagResponse NodeConfig
  deriving (Generic, Newtype)

-- TODO: more type safety.
instance T.NetworkMethod () FindNodeByTagRequest FindNodeByTagResponse where
  toNetworkRequest _ (FindNodeByTagRequest _) = T.MulticastRequest "dummy"
  fromNetworkResponse _ (T.NetworkResponse _) = error "fromNetworkResponse not implemented for FindNodeByTagRequest"
