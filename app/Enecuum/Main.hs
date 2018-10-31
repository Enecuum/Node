module Main where

import           App.Initialize  (initialize)
import           Enecuum.Config  (withConfig)
import           Enecuum.Prelude

defaultConfig :: AsString a => a
defaultConfig = "configs/config.json"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["initialize", configFile] -> withConfig configFile initialize
        _                          -> withConfig defaultConfig initialize
