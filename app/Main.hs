module Main where

import           Enecuum.Config                           ( withConfig )
import           Enecuum.App                              ( initialize )


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["initialize", configFile] -> withConfig configFile initialize
        _                          -> pure ()
