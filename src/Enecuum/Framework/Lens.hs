{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Framework.Lens where

import           Enecuum.Framework.Domain ( NodeConfig )
import           Lens.Micro.TH            ( makeFields )

makeFields ''NodeConfig