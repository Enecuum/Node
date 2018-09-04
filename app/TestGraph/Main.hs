{-# Language ScopedTypeVariables #-}
module Main where


import qualified Enecuum.TGraph as G
import           Enecuum.Dsl.Graph.Interpreter
import           Control.Monad
import           Control.Concurrent.STM
import           Enecuum.Dsl.Graph.Language
import           System.Clock

main :: IO ()
main = testMakeGraph

testMakeGraph :: IO ()
testMakeGraph = do
    aGraph :: TVar (G.TGraph Int) <- initGraph
    forM_ [0 .. 10000] $ \i -> do
        t1 <- getTime Realtime
        runGraph aGraph $ forM_ [l * i + 1 .. l * i + l :: Int] $ \j -> do
            newNode j
            newLinck (j-1) j
        t2 <- getTime Realtime
        putStrLn $ "Time '" ++ show (i*l) ++ "': " ++ show (diffTimeSpec t1 t2)

l = 10000
