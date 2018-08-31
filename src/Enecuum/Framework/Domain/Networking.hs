module Enecuum.Framework.Domain.Networking where

import           Data.Text                                ( Text )

import qualified Enecuum.Core.Types            as T
import           Enecuum.Framework.Domain.Types           ( NodeConfig )

-- Raw vision of networking api. Can change significantly.

newtype FindNodeByTagRequest = FindNodeByTagRequest Text
newtype FindNodeByTagResponse = FindNodeByTagResponse NodeConfig

-- TODO: more type safety.
instance T.NetworkMethod () FindNodeByTagRequest FindNodeByTagResponse where
  toNetworkRequest _ (FindNodeByTagRequest _) = T.MulticastRequest "dummy"
  fromNetworkResponse _ (T.NetworkResponse _) = error "fromNetworkResponse not implemented for FindNodeByTagRequest"

