{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Nodes.NN where

import           Enecuum.Prelude
import qualified Data.Aeson                     as J
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Language               as L
import qualified Enecuum.Assets.Nodes.Address   as A
import qualified Enecuum.Assets.Nodes.Messages  as M
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Framework.Language.Extra (HasStatus)

data NNNodeData = NNNodeData
    { _status   :: D.StateVar L.NodeStatus
    , _netNodes :: D.StateVar (ChordRouteMap D.Address)
    , _nodePort     :: D.StateVar (Maybe D.PortNumber)
    }
makeFieldsNoPrefix ''NNNodeData

newtype Start = Start D.PortNumber

instance J.FromJSON Start where
    parseJSON = J.withObject "Start" $ \o -> Start <$> o J..: "port"

startNode :: NNNodeData -> Start -> L.NodeL Text
startNode nodeData (Start port) = L.atomically $ do
    currentPort <- L.readVar (nodeData^.nodePort)
    unless (isJust currentPort) $
        L.writeVar (nodeData^.nodePort) $ Just port
    pure $ if isJust currentPort
        then "Node is already running."
        else "The port is accepted, the start of the node is started."

initNN :: L.NodeDefinitionL NNNodeData
initNN = L.scenario $ L.atomically
    (NNNodeData <$> L.newVar L.NodeActing <*> L.newVar mempty <*> L.newVar Nothing)

nnNode :: L.NodeDefinitionL ()
nnNode = do
    L.nodeTag "NN node"
    L.logInfo "Starting of BN node"
    nodeData <- initNN
    L.std $ do
        L.stdHandler $ startNode nodeData
        L.stdHandler $ L.stopNodeHandler nodeData
    --L.serving D.Rpc A.bnNodePort $ undefined
    
    L.awaitNodeFinished nodeData