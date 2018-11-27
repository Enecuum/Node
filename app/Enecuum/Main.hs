module Main where

import           App.Initialize  (initialize)
import           App.GenConfigs  (genConfigs)
import           Enecuum.Config  (withConfig)
import           Enecuum.Prelude

defaultConfig :: IsString a => a
defaultConfig = "configs/config.json"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["initialize", configFile] -> withConfig configFile initialize
        ["genConfigs"]             -> genConfigs
        _                          -> withConfig defaultConfig initialize
