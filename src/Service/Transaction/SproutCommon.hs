module Service.Transaction.SproutCommon where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import qualified Data.HashTable.IO                     as H
import           Service.InfoMsg                       (InfoMsg (..))
-- import           Service.Transaction.Common
import           Service.Transaction.Storage
import           Service.Types

data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]
type Number = Integer
type From = Number
type To = Number
type Limit = Integer

data Common = Common {
  db       :: DBPoolDescriptor,
  infoChan :: InChan InfoMsg,
  sprout   :: SproutTable}

type MainChain = HashOfKeyBlock
type SproutChain = HashOfKeyBlock
type Chain = (Maybe MainChain, Maybe SproutChain)
type SproutTable = H.BasicHashTable Number Chain


data SproutException = ValueOfChainIsNotNothing String
                  | NotImplementedException -- test
                  | OtherException
  deriving Show
instance Exception SproutException
