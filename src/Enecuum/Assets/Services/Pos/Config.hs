module Enecuum.Assets.Services.Pos.Config
    ( PosConfig (..)
    ) where

import           Enecuum.Prelude
import           Enecuum.Assets.Services.Pos.Types

data PosConfig = PosConfig
    { _posDefRole :: PosRole
    }