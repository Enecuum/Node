{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import Enecuum.Prelude
import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D

import           Enecuum.Assets.Nodes.Address (graphNodeRpcAddress)
import           Data.HGraph.StringHashable (StringHash (..), toHash)
import           Enecuum.Assets.Nodes.Messages (SuccessMsg (..))

type IterationsCount = Int
type EnableDelays = Bool

data PoWNodeData = PoWNodeData
    { _enableDelays :: EnableDelays
    , _prevHash :: D.StateVar StringHash
    , _prevNumber :: D.StateVar Integer
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
        L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
        sendKBlock kBlock
        when (nodeData ^. enableDelays) $ L.delay $ 1000 * 1000

powNode :: L.NodeDefinitionL ()
powNode = powNode' True 10

powNode' :: EnableDelays -> IterationsCount -> L.NodeDefinitionL ()
powNode' delaysEnabled iterationsCount = do
    L.nodeTag "PoW node"
    L.logInfo "Generating Key Blocks."

    nodeData <- L.initialization $ powNodeInitialization delaysEnabled D.genesisHash

    replicateM_ iterationsCount $ do
        when delaysEnabled $ L.delay $ 1000 * 1000 * 10
        L.scenario $ kBlockProcess nodeData

powNodeInitialization :: EnableDelays -> StringHash -> L.NodeL PoWNodeData
powNodeInitialization delaysEnabled genesisHash = do
  h <- L.atomically $ L.newVar genesisHash
  n <- L.atomically $ L.newVar 1
  pure $ PoWNodeData delaysEnabled h n
