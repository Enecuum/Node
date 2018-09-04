module Enecuum.Legacy.Node.BaseFunctions where

-- import           Control.Exception
import           Universum

undead :: IO a -> IO b -> IO b
undead fin f = finally f (fin >> undead fin f)
