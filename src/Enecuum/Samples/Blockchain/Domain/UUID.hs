{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Samples.Blockchain.Domain.UUID where

import Data.Semigroup
import Data.Word
import Prelude
import Data.Serialize
import Data.UUID (UUID, fromWords, toWords)

uuidG :: Get Word32 -> Get UUID
uuidG word32 = fromWords <$> word32 <*> word32 <*> word32 <*> word32

uuidP :: Putter Word32 -> Putter UUID
uuidP word32 x = case toWords x of
  (a, b, c, d) -> word32 a <> word32 b <> word32 c <> word32 d

instance Serialize UUID where
  put = uuidP put
  get = uuidG get
