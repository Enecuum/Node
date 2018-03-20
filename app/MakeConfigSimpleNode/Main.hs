{-# Language PackageImports #-}
module Main where

import System.Environment (getArgs)
import Network.Socket     (PortNumber)
import Service.Config
import Node.Node.Config.Make
import Node.Data.Data

main :: IO ()
main = do
    args <- getArgs
    maybeConf <- findConfigFile args
    case maybeConf of
      Nothing     -> return ()
      Just config -> do
        maybePort <- getVar config "MakeConfigSimpleNode" "port"
        case maybePort of
          Nothing   -> return ()
          Just port -> makeFileConfig "./data/miningInitData.bin" [BroadcastNode] ((read port)::PortNumber)
