{-# Language PackageImports #-}
module Main where

import Node.Node.Config.Make
import Node.Data.Data

main :: IO ()
main = do
    makeFileConfig "./data/miningInitData.bin" [BroadcastNode] 1666
