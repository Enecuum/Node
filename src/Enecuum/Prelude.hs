{-# OPTIONS -fno-warn-orphans #-}

-- | Uses @universum as default prelude

module Enecuum.Prelude
       ( module Universum
       , readsPrec
       , read
       , B.until
       , show
       , head, last, tail, (!!)
       , at
       ) where

import           Control.Exception (SomeException (..))
import           Control.Lens      (at)
import qualified GHC.Base          as B (until)
import           Text.Read         (read, readsPrec)
import           Text.Show         (show)
import           Universum         hiding (All, Option, Set, Type, head, last,
                                    set, show, tail)
import           Universum.Unsafe  (head, last, tail, (!!))
