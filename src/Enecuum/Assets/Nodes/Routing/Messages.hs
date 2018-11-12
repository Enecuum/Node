{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Enecuum.Assets.Nodes.Routing.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import           Data.HGraph.StringHashable

type NodeId     = StringHash
type PrivateKay = Bool

data HelloToBn = HelloToBn
    { _udpPort   :: D.PortNumber
    , _tcpPort   :: D.PortNumber
    , _rpcPort   :: D.PortNumber
    , _nodeId    :: NodeId
    , _signature :: Bool 
    }
makeFieldsNoPrefix ''HelloToBn

makeHelloToBn
    :: Applicative m
    => PrivateKay -> D.PortNumber -> D.PortNumber -> D.PortNumber -> NodeId -> m HelloToBn
makeHelloToBn _ udpPort' tcpPort' rpcPort' nodeId' =
    pure $ HelloToBn udpPort' tcpPort' rpcPort' nodeId' True

verifyHelloToBn :: HelloToBn -> Bool
verifyHelloToBn _ = True

data HelloToBnResponce = HelloToBnResponce
    { _hostAddress :: D.Host
    , _signature   :: Bool
    }
makeFieldsNoPrefix ''HelloToBnResponce

makeHelloToBnResponce :: Applicative m => PrivateKay -> D.Host ->  m HelloToBnResponce
makeHelloToBnResponce _ host' = pure $ HelloToBnResponce host' True

verifyHelloToBnResponce :: HelloToBnResponce -> Bool
verifyHelloToBnResponce _ = True

data RoutingHello = RoutingHello
    { _hostAddress  :: D.Host
    , _udpPort      :: D.PortNumber
    , _tcpPort      :: D.PortNumber
    , _rpcPort      :: D.PortNumber
    , _nodeId       :: NodeId
    , _signature    :: Bool 
    }
makeFieldsNoPrefix ''RoutingHello

makeRoutingHello
    :: Applicative m
    => PrivateKay -> D.Host -> D.PortNumber -> D.PortNumber -> D.PortNumber -> NodeId -> m RoutingHello
makeRoutingHello _ host' udpPort' tcpPort' rpcPort' nodeId' =
    pure $ RoutingHello host' udpPort' tcpPort' rpcPort' nodeId' True

verifyRoutingHello :: RoutingHello -> Bool
verifyRoutingHello _ = True


