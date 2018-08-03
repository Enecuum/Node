{-# LANGUAGE OverloadedStrings #-}
module Service.Transaction.SproutCommon where

import           Control.Exception
import           Service.Types


data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]
type From = Number
type To = Number
type Limit = Integer


data SproutException = NotImplementedException -- test
                  | OtherException
  deriving Show
instance Exception SproutException
