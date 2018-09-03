module Main where

import           Enecuum.TGraph
import           Control.Monad
import           Control.Monad.STM
import           System.Clock

main :: IO ()
main = print =<< testMakeGraph

testMakeGraph :: IO Int
testMakeGraph = do
    aIndex <- atomically newIndex
    forM_ [0 .. 10000 :: Int] $ \i -> do
        putStrLn $ "Num: " ++ show i
        t1 <- getTime Realtime
        atomically $ forM_ [l * i + 1 .. l * i + l :: Int] $ \i -> do
            void $ newTNode aIndex i i
            aNode1 <- findNode (i - 1) aIndex
            aNode2 <- findNode i aIndex
            case (aNode1, aNode2) of
                (Just aJustNode1, Just aJustNode2) ->
                    addLinck aJustNode1 aJustNode2
                _ -> return ()

        t2 <- getTime Realtime
        putStrLn $ "Insert: " ++ show (diffTimeSpec t1 t2)

    t1  <- getTime Realtime
    res <- atomically $ foldGraph 0 (+) aIndex
    t2  <- getTime Realtime
    putStrLn $ "Sum: " ++ show (diffTimeSpec t1 t2)
    return res

l = 10000
