{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import Enecuum.Prelude
import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D

import           Enecuum.Assets.Nodes.Address (graphNodeRpcAddress)
import           Data.HGraph.StringHashable (StringHash (..))
import           Enecuum.Assets.Nodes.Messages (
    SuccessMsg (..), ForeverChainGeneration(..), NBlockPacketGeneration(..))

type IterationsCount = Int
type EnableDelays = Bool

data PoWNodeData = PoWNodeData
    { _enableDelays        :: EnableDelays
    , _prevHash            :: D.StateVar StringHash
    , _prevNumber          :: D.StateVar Integer
    , _requiredBlockNumber :: D.StateVar Int
    }

makeFieldsNoPrefix ''PoWNodeData

data KeyBlockRequest  = KeyBlockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KeyBlockResponse = KeyBlockResponse { kBlock :: [D.KBlock] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

sendKBlock :: D.KBlock -> L.NodeL ()
sendKBlock kBlock = do
    eResult <- L.makeRpcRequest graphNodeRpcAddress kBlock
    case eResult of
      Left msg -> L.logInfo $ "KBlock sending failed: " +|| msg ||+ "."
      Right SuccessMsg -> L.logInfo "KBlock sending success."

kBlockProcess :: PoWNodeData -> L.NodeL ()
kBlockProcess nodeData = do
    prevKBlockHash   <- L.atomically <$> L.readVar $ nodeData ^. prevHash
    prevKBlockNumber <- L.atomically <$> L.readVar $ nodeData ^. prevNumber

    (lastHash, kBlocks) <- D.generateKBlocks prevKBlockHash prevKBlockNumber
    -- L.logInfo $ "KBlocks generated: " +|| kBlocks ||+ "."
    L.logInfo $ "Last hash: " +|| lastHash ||+ "."

    L.atomically $ L.writeVar (nodeData ^. prevHash) lastHash
    L.atomically $ L.writeVar (nodeData ^. prevNumber) $ prevKBlockNumber + (fromIntegral $ length kBlocks)

    forM_ kBlocks $ \kBlock -> do
        L.logInfo $ "Sending KBlock: " +|| kBlock ||+ "."
        sendKBlock kBlock
        when (nodeData ^. enableDelays) $ L.delay $ 1000 * 1000

foreverChainGenerationHandle
    :: PoWNodeData -> ForeverChainGeneration -> L.NodeL SuccessMsg
foreverChainGenerationHandle powNodeData _ = do
    L.atomically $ L.writeVar (powNodeData^.requiredBlockNumber) (10^100)
    pure SuccessMsg 

nBlockPacketGenerationHandle
    :: PoWNodeData -> NBlockPacketGeneration -> L.NodeL SuccessMsg
nBlockPacketGenerationHandle powNodeData (NBlockPacketGeneration i) = do
    L.atomically $ L.modifyVar (powNodeData^.requiredBlockNumber) (+i)
    pure SuccessMsg 

powNode :: L.NodeDefinitionL ()
powNode = powNode' True 10

powNode' :: EnableDelays -> IterationsCount -> L.NodeDefinitionL ()
powNode' delaysEnabled iterationsCount = do
    L.nodeTag "PoW node"
    L.logInfo "Generating Key Blocks."

    nodeData <- L.initialization $ powNodeInitialization delaysEnabled D.genesisHash
    L.serving 2005 $ do
        L.method $ foreverChainGenerationHandle nodeData
        L.method $ nBlockPacketGenerationHandle nodeData
    forever $ do
        when delaysEnabled $ L.delay $ 1000 * 1000
        ok <- L.scenario $ L.atomically $ do
            i <- L.readVar (nodeData^.requiredBlockNumber)
            when (i > 0) $ L.writeVar (nodeData^.requiredBlockNumber) (i - 1)
            pure $ i > 0
        when ok $ L.scenario $ kBlockProcess nodeData

powNodeInitialization :: EnableDelays -> StringHash -> L.NodeL PoWNodeData
powNodeInitialization delaysEnabled genesisHash = do
  h <- L.atomically $ L.newVar genesisHash
  n <- L.atomically $ L.newVar 1
  b <- L.atomically $ L.newVar 0
  pure $ PoWNodeData delaysEnabled h n b
