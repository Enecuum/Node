module Main where

import           App.Initialize  (initialize)
import           Enecuum.Config  (withConfig)
import           Enecuum.Prelude

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["initialize", configFile] -> withConfig configFile initialize
        _                          -> withConfig "configs/config.json" initialize
