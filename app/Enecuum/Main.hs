module Main where

import           App.GenConfigs  (genConfigs)
import           App.Initialize  (initialize)
import           Enecuum.Config  (withConfig)
import           Enecuum.Prelude

defaultConfig :: IsString a => a
defaultConfig = "configs/config.json"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["singlenode", configFile] -> withConfig configFile initialize
        ["generate-configs"]       -> genConfigs
        _                          -> withConfig defaultConfig initialize
