{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import Enecuum.Prelude
import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D

import           Enecuum.Assets.Nodes.Address (grpahNodeRpcAddress)
import           Data.HGraph.StringHashable (StringHash (..))
import           Enecuum.Assets.Nodes.Messages (SuccessMsg (..))

data PoWNodeData = PoWNodeData
    { _prevHash :: D.StateVar StringHash
    , _prevNumber :: D.StateVar Integer
    }

makeFieldsNoPrefix ''PoWNodeData

data KeyBlockRequest  = KeyBlockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KeyBlockResponse = KeyBlockResponse { kBlock :: [D.KBlock] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

sendKBlock :: D.KBlock -> L.NodeL ()
sendKBlock kBlock = do
    eResult <- L.makeRpcRequest grpahNodeRpcAddress kBlock
    case eResult of
      Left msg -> L.logInfo $ "KBlock sending failed: " +|| msg ||+ "."
      Right SuccessMsg -> L.logInfo "KBlock sending success."

kBlockProcess :: PoWNodeData -> L.NodeL ()
kBlockProcess nodeData = do
    prevKBlockHash   <- L.atomically <$> L.readVar $ nodeData ^. prevHash
    prevKBlockNumber <- L.atomically <$> L.readVar $ nodeData ^. prevNumber
    
    (lastHash, kBlocks) <- D.generateKBlocks prevKBlockHash prevKBlockNumber
    L.logInfo $ "KBlocks generated: " +|| kBlocks ||+ "."
    L.logInfo $ "Last hash: " +|| lastHash ||+ "."

    L.atomically $ L.writeVar (nodeData ^. prevHash) lastHash
    L.atomically $ L.writeVar (nodeData ^. prevNumber) $ fromIntegral $ length kBlocks

    forM_ kBlocks $ \kBlock -> do
        L.logInfo $ "Sending KBlock: " +|| kBlock ||+ "."
        sendKBlock kBlock
        L.delay $ 1000 * 1000


powNode :: L.NodeDefinitionL ()
powNode = do
    L.nodeTag "PoW node"
    L.logInfo "Generating Key Blocks."

    nodeData <- L.initialization $ powNodeInitialization D.genesisHash

    forever $ do
        L.delay $ 1000 * 1000 * 10
        L.scenario $ kBlockProcess nodeData

powNodeInitialization :: StringHash -> L.NodeL PoWNodeData
powNodeInitialization genesisHash = do
  h <- L.atomically $ L.newVar genesisHash
  n <- L.atomically $ L.newVar 0
  pure $ PoWNodeData h n
