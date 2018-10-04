{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D
import           Enecuum.Prelude
import Enecuum.Assets.Nodes.Address (powAddr)
import           Data.HGraph.StringHashable (toHash)
import Data.Set (deleteAt, fromList, elemAt)
import Data.List (delete)

data PoWNodeData = PoWNodeData
    { _kBlocks :: D.StateVar [D.KBlock]
    }

makeFieldsNoPrefix ''PoWNodeData

data KeyBlockRequest  = KeyBlockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KeyBlockResponse = KeyBlockResponse { kBlock :: D.KBlock }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


getRandomKBlock :: PoWNodeData -> KeyBlockRequest -> L.NodeL KeyBlockResponse
getRandomKBlock nodeData KeyBlockRequest =  do
  L.logInfo "Choose randomly Key Block"
  -- result <- L.atomically $ do
  --   blocks <- L.readVar $ (nodeData ^. kBlocks)
  --   let maxRange = fromIntegral $ length blocks
  --   r <- fromIntegral <$> L.getRandomInt (1, maxRange)
  --   let kblock = blocks !! r
  --   L.WriteVar (nodeData ^. kBlocks) (delete kblock blocks)
  --   pure kblock
  -- pure $ KeyBlockResponse result
  undefined

-- powNode :: L.NodeDefinitionL ()
powNode = do
    L.nodeTag "PoW node"
    L.logInfo "Generate Key Block"
    let (D.Address _ port) = powAddr
    forM_ [1, 6, 11, 16] (\k -> do
      nodeData <- L.initialization $ powNodeInitialization k
      L.servingRpc port $ do
        L.method $ getRandomKBlock nodeData)

powNodeInitialization :: Integer -> L.NodeL PoWNodeData
powNodeInitialization k = do
    let numbers = [k..k+5]
        blocks = map genKBlocks numbers
    kblocks <- L.atomically $ L.newVar blocks
    pure $ PoWNodeData kblocks


genKBlocks :: Integer -> D.KBlock
genKBlocks i = D.KBlock
    { _prevHash   = toHash i
    , _number     = i
    , _nonce      = i
    , _solver     = toHash (i + 3)
    }
