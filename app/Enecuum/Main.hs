module Main where

import           App.Initialize  (initialize, runMultiNode)
import           Enecuum.Config  (withConfig)
import           Enecuum.Prelude

defaultConfig :: IsString a => a
defaultConfig = "configs/config.json"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["initialize", configFile] -> withConfig configFile initialize
        ["m", configFile]          -> withConfig configFile runMultiNode
        _                          -> withConfig defaultConfig initialize
