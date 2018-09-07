module Enecuum.Legacy.Node.BaseFunctions where


import           Enecuum.Prelude

undead :: IO a -> IO b -> IO b
undead fin f = finally f (fin >> undead fin f)
