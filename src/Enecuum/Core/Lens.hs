{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Core.Lens
  ( module X
  ) where

import Enecuum.Core.HGraph.Lens as X
import Enecuum.Core.Logger.Runtime
import Enecuum.Core.Runtime
import Control.Lens             ( makeFieldsNoPrefix )

makeFieldsNoPrefix ''CoreRuntime