{-# Language PackageImports #-}
module Main where

import Node.Node.Config.Make
import Node.Data.Data

main :: IO ()
main = do
    makeFileConfig "./data/bootInitData.bin" [BootNode] 1666
