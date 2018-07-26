{-# LANGUAGE OverloadedStrings #-}
module Service.Transaction.SproutCommon where

-- import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
-- import qualified Data.HashTable.IO                     as H
-- import qualified Data.Serialize                        as S (decode, encode)
-- import           Service.InfoMsg                       (InfoMsg (..))
import           Service.Transaction.Storage
import           Service.Types


data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]
type From = Number
type To = Number
type Limit = Integer


-- type MainChain = HashOfKeyBlock
-- type SproutChain = HashOfKeyBlock
-- type Chain = (Maybe MainChain, Maybe SproutChain)
-- type FullChain = (Integer, Maybe MainChain, Maybe SproutChain)

-- type SproutTable = H.BasicHashTable Number Chain


data SproutException = NotImplementedException -- test
                  | OtherException
  deriving Show
instance Exception SproutException
