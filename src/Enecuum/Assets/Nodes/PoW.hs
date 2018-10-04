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
import           System.Random

data Ordering = InOrder | RandomOrder
data PoWNodeData = PoWNodeData
    { _kBlocks :: D.StateVar [D.KBlock]
    , _prevHash :: StringHash
    }

makeFieldsNoPrefix ''PoWNodeData

data KeyBlockRequest  = KeyBlockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KeyBlockResponse = KeyBlockResponse { kBlock :: [D.KBlock] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


kBlockInBunch :: Integer  
kBlockInBunch = 5  

sendKBlock :: PoWNodeData -> KeyBlockRequest -> L.NodeL KeyBlockResponse
sendKBlock nodeData KeyBlockRequest = do
  kblocks <- L.atomically $ L.readVar $ nodeData ^. kBlocks
  pure $ KeyBlockResponse kblocks


-- someNode = do
--   kBlockBunch <- generateKBlocks prevKBlockHash 5
--   kBlockIndices <- generateIndices RandomOrder 5   -- [0..4]
--   forM_ send


generateKBlocks :: StringHash -> Integer -> Free L.NodeDefinitionF [D.KBlock]
generateKBlocks _ from = do
  pure $ map genKBlocks [from .. from + kBlockInBunch]

loopGenIndices :: [Integer] -> Free L.NodeDefinitionF [Integer]
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

generateIndices :: Ordering -> Free L.NodeDefinitionF [Integer]
generateIndices order = do
  case order of
    RandomOrder -> loopGenIndices [0 .. kBlockInBunch]
    InOrder -> pure $ [0 .. kBlockInBunch]  

powNode :: L.NodeDefinitionL ()
powNode = do
    L.nodeTag "PoW node"
    L.logInfo "Generate Key Block"
    let (D.Address _ port) = powAddr
    -- initialize with genesis hash
    forM_ [0, kBlockInBunch ..] (\from -> do
      -- prevKBlockHash <- L.atomically <$> L.readVar $ nodeData ^. prevHash
      let prevKBlockHash = undefined
      kBlockBunch <- generateKBlocks prevKBlockHash from 
      kBlockIndices <- generateIndices RandomOrder 
      let kBlocks = map ((kBlockBunch !! )  . fromIntegral) kBlockIndices 
      let genesisHash = undefined
      nodeData <- L.initialization $ powNodeInitialization genesisHash kBlocks
      L.servingRpc port $ L.method $ sendKBlock nodeData)

powNodeInitialization :: StringHash -> [D.KBlock] -> L.NodeL PoWNodeData
powNodeInitialization genesisHash kblocks = do
  b <- L.atomically $ L.newVar kblocks
  pure $ PoWNodeData b genesisHash


genKBlocks :: Integer -> D.KBlock
genKBlocks i = D.KBlock
    { _prevHash   = toHash i
    , _number     = i
    , _nonce      = i
    , _solver     = toHash (i + 3)
    }
