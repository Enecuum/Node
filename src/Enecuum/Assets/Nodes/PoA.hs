{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoA where

import           Enecuum.Assets.Nodes.Address
import           Data.HGraph.StringHashable   (toHash)
import           Enecuum.Assets.Nodes.Address (poaAddr)
import qualified Enecuum.Domain               as D
import qualified Enecuum.Language             as L
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.Messages

data PoANodeData = PoANodeData {
        _currentLastKeyBlock :: D.StateVar D.KBlock
    }


makeFieldsNoPrefix ''PoANodeData


poaNode :: L.NodeDefinitionL ()
poaNode = do
    L.nodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    L.scenario $ do
        poaData <- L.atomically (PoANodeData <$> L.newVar D.genesisKBlock)
        forever $ do
            L.delay $ 100 * 1000
            kBlock <- L.makeRpcRequest graphNodeRpcAddress GetLastKBlock
            case kBlock of
                Left _ -> return ()
                Right block -> do
                    currentBlock <- L.atomically $ L.readVar (poaData^.currentLastKeyBlock)
                    when (block /= currentBlock) $ do
                        L.atomically $ L.writeVar (poaData^.currentLastKeyBlock) block
                        mBlock <- D.genRandMicroblock block
                        _ :: Either Text SuccessMsg <- L.makeRpcRequest graphNodeRpcAddress mBlock
                        pure ()