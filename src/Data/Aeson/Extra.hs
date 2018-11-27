module Data.Aeson.Extra where

import Prelude (drop, Bool (..))
import Data.Aeson
    (   Options
    ,   defaultOptions
    ,   fieldLabelModifier
    ,   unwrapUnaryRecords
    ,   tagSingleConstructors
    )

noLensPrefix :: Options
noLensPrefix = defaultOptions { fieldLabelModifier = drop 1 }

noLensPrefixJsonConfig :: Options
noLensPrefixJsonConfig = noLensPrefix
    { unwrapUnaryRecords    = False
    , tagSingleConstructors = True
    }