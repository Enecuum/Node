module Enecuum.Framework.Domain.Tags where

import           Universum
import qualified Data.Text          as T
import           Data.Typeable

toTag :: Typeable a => a -> Text
toTag = T.pack . takeWhile (/= ' ') . show . typeOf