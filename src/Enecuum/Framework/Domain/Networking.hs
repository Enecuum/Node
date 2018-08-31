{-# LANGUAGE DeriveGeneric #-}

module Enecuum.Framework.Domain.Networking where

import           Data.Text                                ( Text )

import qualified Enecuum.Core.Types            as T
import           Enecuum.Framework.Domain.Types           ( NodeConfig )
import           GHC.Generics                             ( Generic )
import           Control.Newtype.Generics                 ( Newtype
                                                          )

-- Raw vision of networking api. Can change significantly.

newtype FindNodeByTagRequest = FindNodeByTagRequest Text
newtype FindNodeByTagResponse = FindNodeByTagResponse NodeConfig
  deriving (Generic)

instance Newtype FindNodeByTagResponse

-- TODO: more type safety.
instance T.NetworkMethod () FindNodeByTagRequest FindNodeByTagResponse where
  toNetworkRequest _ (FindNodeByTagRequest _) = T.MulticastRequest "dummy"
  fromNetworkResponse _ (T.NetworkResponse _) = error "fromNetworkResponse not implemented for FindNodeByTagRequest"
