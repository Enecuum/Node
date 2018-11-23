module Enecuum.Assets.Nodes.TstNodes.GraphNode.Initialization where

import qualified Data.Map                                       as Map
import           Enecuum.Assets.Nodes.GraphNode.Logic
import qualified Enecuum.Assets.Nodes.TstNodes.GraphNode.CLens  as CLens
import           Enecuum.Assets.Nodes.TstNodes.GraphNode.Config
import qualified Enecuum.Assets.System.Directory                as L
import qualified Enecuum.Blockchain.DB                          as D
import qualified Enecuum.Domain                                 as D
import qualified Enecuum.Language                               as L
import           Enecuum.Prelude
import           System.FilePath                                ((</>))

type TstGraphNodeData = GraphNodeData' TstGraphNode

initTstDBModel :: NodeConfig TstGraphNode -> L.NodeL (Maybe D.DBModel)
initTstDBModel nodeConfig = do

    parentDir <- if nodeConfig ^. CLens.useEnqHomeDir
        then L.getEnecuumDir
        else pure ""

    let dbModelPath = parentDir </> (nodeConfig ^. CLens.dbModelName)
    initDBModel' dbModelPath $ nodeConfig ^. CLens.dbOptions

-- | Initialization of graph node
graphNodeTstInitialization :: NodeConfig TstGraphNode -> L.NodeDefinitionL (Either Text TstGraphNodeData)
graphNodeTstInitialization nodeConfig = L.scenario $ do
    let useDb       = nodeConfig ^. CLens.useDatabase
    let stopOnDbErr = nodeConfig ^. CLens.stopOnDatabaseError

    mbDBModel <- if useDb
        then initTstDBModel nodeConfig
        else pure Nothing

    g <- L.newGraph
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock

    nodeData <- L.atomically
        $  GraphNodeData' <$>
            ( D.BlockchainData g
                <$> L.newVar Map.empty
                <*> L.newVar Map.empty
                <*> L.newVar D.genesisHash
                <*> L.newVar Map.empty
            )
        <*> L.newVar L.NodeActing
        <*> pure nodeConfig
        <*> pure mbDBModel
        <*> L.newVar False
        <*> L.newVar False
        <*> L.newVar False

    let dbUsageFailed = useDb && stopOnDbErr && isNothing mbDBModel

    unless dbUsageFailed
        $ L.logInfo $ "Genesis block (" +|| D.genesisHash ||+ "): " +|| D.genesisKBlock ||+ "."

    if dbUsageFailed
        then pure $ Left "Database error."
        else pure $ Right nodeData
