module Enecuum.Framework.Testing.Interpreters where

import           Data.Text                                ( Text )
import           Eff                                      ( Eff )
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( ToJSON
                                                          , FromJSON
                                                          )
import qualified Data.ByteString.Lazy          as BS
import           GHC.Generics                             ( Generic )
import           Control.Newtype.Generics                 ( Newtype
                                                          , O
                                                          , pack
                                                          , unpack
                                                          )

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
-- import qualified Enecuum.Framework.Runtime     as R
