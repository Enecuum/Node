{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}

module Enecuum.Assets.Nodes.GraphNode.Transmitter where

import           Enecuum.Prelude
import Data.Map (fromList, lookup, insert, Map(..), elems, keys, empty)
import           Control.Lens                  (makeFieldsNoPrefix)


import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Data.HGraph.StringHashable

import           Enecuum.Framework.Language.Extra (HasGraph, HasStatus, NodeStatus (..))

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Address

import qualified Enecuum.Framework.LogState as Log
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.GraphNode.Logic

-- | Start of graph node
graphNodeTransmitter :: L.NodeDefinitionL ()
graphNodeTransmitter = do
    L.nodeTag "graphNodeTransmitter"
    nodeData <- graphNodeInitialization

    L.serving D.Tcp graphNodeTransmitterTcpPort $ do
        L.handler $ acceptMBlock nodeData
        L.handler $ acceptKBlock nodeData
        L.handler $ methodPing

    L.serving D.Rpc graphNodeTransmitterRpcPort $ do
        L.method  $ getLastKBlock nodeData
        L.method  $ getKBlockPending nodeData
        L.method  $ getTransactionPending nodeData        
        L.methodE $ getBalance nodeData
        L.method  $ getChainLength nodeData
        L.methodE $ acceptChainFromTo nodeData
        L.methodE $ getMBlockForKBlocks nodeData
        L.method  $ rpcPingPong
        L.method  $ methodStopNode nodeData
        L.methodE $ acceptTransaction nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.awaitNodeFinished nodeData
