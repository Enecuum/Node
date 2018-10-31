{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Nodes.BN where

import           Enecuum.Prelude
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Language               as L
import qualified Enecuum.Assets.Nodes.Address   as A
import qualified Enecuum.Assets.Nodes.Messages  as M
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Framework.Language.Extra (HasStatus)

data BNNodeData = BNNodeData
    { _status   :: D.StateVar L.NodeStatus
    , _netNodes :: D.StateVar (ChordRouteMap D.Address)
    }

makeFieldsNoPrefix ''BNNodeData

initBN :: L.NodeDefinitionL BNNodeData
initBN = L.scenario $ L.atomically
    (BNNodeData <$> L.newVar L.NodeActing <*> L.newVar mempty)

acceptNewNode :: BNNodeData -> M.Hello -> L.NodeL M.SuccessMsg
acceptNewNode nodeData (M.Hello hash address) = do
    void $ L.atomically $
        L.modifyVar (nodeData ^. netNodes) $ addToMap hash address
    pure M.SuccessMsg

findConnects :: BNNodeData -> M.ConnectRequest -> L.NodeL (Either Text D.Address)
findConnects nodeData (M.ConnectRequest hash i) = do
    address <- L.atomically $ do
        connectMap <- L.readVar (nodeData ^. netNodes)
        pure $ findInMapNByKey
            (\h num -> (D.hashToInteger h + 2 ^ num) `mod` quantityOfHashes)
            i
            hash
            connectMap
    pure $ maybe (Left "Connect map is empty.") (Right . snd) address


bnNode :: L.NodeDefinitionL ()
bnNode = do
    L.nodeTag "BN node"
    L.logInfo "Starting of BN node"
    nodeData <- initBN
    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.serving D.Rpc A.bnNodePort $ do
        L.method  $ acceptNewNode nodeData
        L.methodE $ findConnects  nodeData

    L.awaitNodeFinished nodeData