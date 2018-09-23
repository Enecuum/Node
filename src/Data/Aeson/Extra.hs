module Data.Aeson.Extra where

import Prelude (drop)
import Data.Aeson (Options, defaultOptions, fieldLabelModifier)

noLensPrefix :: Options
noLensPrefix = defaultOptions { fieldLabelModifier = drop 1 }
