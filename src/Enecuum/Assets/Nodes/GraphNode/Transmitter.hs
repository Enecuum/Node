{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiWayIf             #-}

module Enecuum.Assets.Nodes.GraphNode.Transmitter where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Assets.Nodes.Address
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
