{-# Language PackageImports #-}
module Main where

import System.Environment (getArgs)
import Network.Socket     (PortNumber)
import Service.Config
import Node.Node.Config.Make
import Node.Data.NodeTypes

main :: IO ()
main = do
    args <- getArgs
    maybeConf <- findConfigFile args
    case maybeConf of
      Nothing     -> return ()
      Just config -> do
        maybePort <- getVar config "MakeConfigBootNode" "port"
        case maybePort of
          Nothing -> return ()
          Just port -> makeFileConfig "./data/bootInitData.bin" [BootNode] ((read port)::PortNumber)
