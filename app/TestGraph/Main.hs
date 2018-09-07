{-# Language ScopedTypeVariables #-}
module Main where


import qualified Enecuum.Research.THashGraph as G
import           Enecuum.Research.Dsl.HashGraph.Interpreter

import           Control.Concurrent.STM
import           Enecuum.Research.Dsl.HashGraph.Language
import           System.Clock
import           Enecuum.Prelude

main :: IO ()
main = testMakeGraph

testMakeGraph :: IO ()
testMakeGraph = do
    aGraph :: TVar (G.THashGraph Int) <- initHashGraph
    forM_ [0 .. 10000] $ \i -> do
        t1 <- getTime Realtime
        runHashGraph aGraph $ forM_ [l * i + 1 .. l * i + l :: Int] $ \j -> do
            newNode j
            newLink (j-1) j
        t2 <- getTime Realtime
        putStrLn $ "Time '" ++ show (i*l) ++ "': " ++ show (diffTimeSpec t1 t2)

l = 10000
