module Main where

import           Enecuum.Samples.Assets.GenConfigs     (genConfigs)
import           Enecuum.Samples.Assets.Initialization (initialize)
import           Enecuum.Config                (withConfig)
import           Enecuum.Prelude

help :: IO ()
help = putStrLn @Text $ "Please, specify node config:"
    <> "\n\n$ enq-node-haskell singlenode configs/tst_graph_node_transmitter.json"
    <> "\n\nOr generate default configs (they will be placed to ./configs/default):"
    <> "\n\n$ enq-node-haskell generate-configs"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["singlenode", configFile] -> withConfig configFile initialize
        ["generate-configs"]       -> genConfigs
        _                          -> help
