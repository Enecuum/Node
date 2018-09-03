{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Core.Lens where

import           Enecuum.Core.Types ( NetworkRequest, NetworkResponse )
import           Lens.Micro.TH      ( makeFields )


makeFields ''NetworkRequest
makeFields ''NetworkResponse