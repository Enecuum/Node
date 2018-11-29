{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Enecuum.Assets.Nodes.Routing.Messages where

import           Data.HGraph.StringHashable
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Domain               (NodeAddress (..), NodeId (..), NodePorts (..))
import qualified Enecuum.Domain               as D
import           Enecuum.Prelude

type PrivateKay = Bool

data HelloToBn = HelloToBn
    { _senderPorts :: NodePorts
    , _senderId    :: NodeId
    , _signature   :: Bool
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeFieldsNoPrefix ''HelloToBn

makeHelloToBn :: Applicative m => PrivateKay -> NodePorts -> NodeId -> m HelloToBn
makeHelloToBn _ nodePorts' nodeId' = pure $ HelloToBn nodePorts' nodeId' True

verifyHelloToBn :: HelloToBn -> Bool
verifyHelloToBn _ = True

newtype AddressRequest = AddressRequest NodeId deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RoutingHello = RoutingHello
    { _nodeAddress :: NodeAddress
    , _signature   :: Bool
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
makeFieldsNoPrefix ''RoutingHello

makeRoutingHello :: Applicative m => PrivateKay -> NodeAddress -> m RoutingHello
makeRoutingHello _ nodeAddress' = pure $ RoutingHello nodeAddress' True

verifyRoutingHello :: RoutingHello -> Bool
verifyRoutingHello _ = True

newtype NextForYou = NextForYou D.Address
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SendMsgTo = SendMsgTo
    { _nodeReceiverId :: NodeId
    , _timeToLive     :: Int
    , _msg            :: Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeFieldsNoPrefix ''SendMsgTo
