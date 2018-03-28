module Boot.Map.Random (
    RandomMap (..),
    empty,
    insert,
    delete,
    Boot.Map.Random.lookup,
    keys,
    takeRandom
  ) where

import              System.Random
import              Control.Monad
import              Data.Maybe
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import qualified    Data.Map as M
import              Node.Data.Data

data RandomMap i = RandomMap NodeId (M.Map NodeId i)

empty :: RandomMap i
empty = RandomMap (NodeId 0) M.empty

insert :: NodeId -> i -> RandomMap i -> RandomMap i
insert nId i (RandomMap e rMap) = RandomMap (max nId e) (M.insert nId i rMap)

delete :: NodeId -> RandomMap i -> RandomMap i
delete nId (RandomMap e rMap) = RandomMap e (M.delete nId rMap)

lookup :: NodeId -> RandomMap i -> Maybe i
lookup i (RandomMap _ rMap) = i `M.lookup` rMap

keys :: RandomMap i -> [NodeId]
keys (RandomMap _ rMap) = M.keys rMap

takeRandom :: Int -> RandomMap a -> IO [(NodeId, a)]
takeRandom i (RandomMap e rMap) = takeRandomInternal i  e rMap

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

takeRandomInternal :: Int -> NodeId -> M.Map NodeId a -> IO [(NodeId, a)]
takeRandomInternal n (NodeId g) dataSet = do
    randList <- forM [1..n] $ \_ -> do
        key <- randomRIO (9, g)
        pure $ (NodeId key) `M.lookupLE` dataSet
    pure $ catMaybes randList
