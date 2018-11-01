module Enecuum.Assets.Nodes.GraphNode.Database where

import           Enecuum.Prelude

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.DB         as DB

import           Enecuum.Config
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.GraphNode.Config

loadKBlock :: GraphNodeData -> D.DBValue DB.KBlockMetaEntity -> L.NodeL (D.DBResult D.KBlock)
loadKBlock = error ""

loadHashMeta :: GraphNodeData -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue DB.KBlockMetaEntity))
loadHashMeta = error ""

loadNextKBlock :: GraphNodeData -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock nodeData prevHash = do
    eHashMeta <- loadHashMeta nodeData prevHash
    either (pure . Left) (loadKBlock nodeData) eHashMeta

restoreFromDB :: GraphNodeData -> L.NodeL ()
restoreFromDB nodeData = do
    eKBlock <- loadNextKBlock nodeData D.genesisIndicationHash

    pure ()