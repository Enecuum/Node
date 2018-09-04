module Enecuum.Legacy.Service.Transaction.Skelet (getSkeletDAG) where

import           Control.Monad.Trans.Class
-- import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Prelude                    (tail)
import           System.Random
import           Universum                  hiding (tail)
--type Skelet = [(Node, Node)]

completeGraph :: [a] -> [(a,a)]
completeGraph []            = []
completeGraph [_]        = []
completeGraph [x1,_,x3] = [(x1,x3)]
completeGraph (x1:x2:xs)    = zip (repeat x1) (take 3 xs) ++ completeGraph (x2:xs)

getSkeletDAG :: [a] -> [(a,a)]
getSkeletDAG as = let graph = completeGraph as
                      gen   = mkStdGen 42
                     -- gen   = globalStdGen
                      skel   = mkSkel graph (0 :: Int, 1 :: Int)
                  in zip as (tail as) ++ execWriter (runStateT skel gen)

mkSkel :: (Random b, Eq b, Num b, RandomGen g) => [(a,a)] -> (b, b) -> StateT g (Writer [(a,a)]) ()
mkSkel [] _         = return ()
mkSkel (e:es) (x,y) = do
  gen1 <- get
  let (a, gen2) = randomR (x,y) gen1
  put gen2
  if a == 0
  then do lift $ tell [e]
          mkSkel es (x,y)
  else mkSkel es (x,y)
