{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D
import           Enecuum.Prelude hiding (Ordering)
import Enecuum.Assets.Nodes.Address (powAddr)
import           Data.HGraph.StringHashable (StringHash (..),toHash)
import Data.List (delete)

data Ordering = InOrder | RandomOrder
data PoWNodeData = PoWNodeData
    {
      _prevHash :: D.StateVar StringHash
    }

makeFieldsNoPrefix ''PoWNodeData

data KeyBlockRequest  = KeyBlockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KeyBlockResponse = KeyBlockResponse { kBlock :: [D.KBlock] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)



-- Create and send bunch of kblocks
sendKBlock :: PoWNodeData -> Integer -> KeyBlockRequest -> L.NodeL KeyBlockResponse
sendKBlock nodeData from KeyBlockRequest = do
  prevKBlockHash <- L.atomically <$> L.readVar $ nodeData ^. prevHash
  kBlockBunch <- D.generateKBlocks prevKBlockHash from
  kBlockIndices <- generateIndices RandomOrder
  let kBlocks = map ((kBlockBunch !! )  . fromIntegral) kBlockIndices
  pure $ KeyBlockResponse kBlocks

generateIndices :: Ordering -> Free L.NodeF [Integer]
generateIndices order = do
  case order of
    RandomOrder -> loopGenIndices [0 .. D.kBlockInBunch]
    InOrder -> pure $ [0 .. D.kBlockInBunch]

-- loop: choose randomly one from the rest of list Integers
-- example:
-- [1,2,3,4,5] - 2
-- [1,3,4,5] - 4
-- [1,3,5] - 5
-- [1,3] - 1
-- [3] - 3
-- the result: [2,4,5,1,3]
loopGenIndices :: [Integer] -> L.NodeL [Integer]
loopGenIndices numbers = do
  if (not $ null numbers)
    then do
      let maxIndex = fromIntegral $ length numbers - 1
      p <- fromIntegral <$> L.getRandomInt (0, maxIndex)
      let result = numbers !! p
      -- choose next number from rest
      rest <- loopGenIndices $ delete result numbers
      return (result:rest)
    else return []

powNode :: L.NodeDefinitionL ()
powNode = do
    L.nodeTag "PoW node"
    L.logInfo "Generate Key Block"
    let (D.Address _ port) = powAddr
    -- initialize with genesis hash
    nodeData <- L.initialization $ powNodeInitialization D.genesisHash

    forM_ [0, D.kBlockInBunch ..] (\from -> do

      L.serving port $ L.method $ sendKBlock nodeData from)

powNodeInitialization :: StringHash -> L.NodeL PoWNodeData
powNodeInitialization genesisHash = do
  h <- L.atomically $ L.newVar genesisHash
  pure $ PoWNodeData h

