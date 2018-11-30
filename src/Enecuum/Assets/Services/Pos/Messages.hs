{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Services.Pos.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Framework.Domain                                   as D
import qualified Data.HGraph.StringHashable                                 as D
import           Enecuum.Assets.Services.Pos.Types
import           Enecuum.Assets.Services.Routing

data MyRoleIs = MyRoleIs PosRole D.StringHash D.NodeId
    deriving (Show, Eq, Generic, Serialize, ToJSON, FromJSON)

newtype IAmNewPos = IAmNewPos D.NodeId
    deriving (Show, Eq, Generic, Serialize, ToJSON, FromJSON)

--
data IAmPos = IAmPos
    { _nodeReceiverId :: D.NodeId
    , _timeToLive     :: Int
    , _msg            :: IAmNewPos
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeFieldsNoPrefix ''IAmPos

data ShadowRequest = ShadowRequest D.StringHash D.NodeId
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialize)

data ShadowResponce = ShadowResponce
    { _nodeReceiverId :: D.NodeId
    , _timeToLive     :: Int
    , _msg            :: D.NodeId
    } deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialize)

makeFieldsNoPrefix ''ShadowResponce

data LeaderBeacon = LeaderBeacon D.StringHash D.NodeId
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialize)