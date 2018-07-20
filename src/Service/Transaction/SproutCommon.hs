module Service.Transaction.SproutCommon where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import qualified Data.HashTable.IO                     as H
import           Service.InfoMsg                       (InfoMsg (..))
import           Service.Transaction.Common
import           Service.Types


data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]
type Number = Integer
type From = Number
type To = Number

data Common = Common {
  db       :: DBPoolDescriptor,
  infoChan :: InChan InfoMsg,
  sprout   :: SproutTable}

data BranchOfChain = Main | Sprout

type MainChain = HashOfKeyBlock
type SproutChain = HashOfKeyBlock
type Chain = (Maybe MainChain, Maybe SproutChain)
type SproutTable = H.BasicHashTable Number Chain


data SproutException = SproutExists String
                  | NotImplementedException -- test
                  | OtherException
  deriving Show
instance Exception SproutException
