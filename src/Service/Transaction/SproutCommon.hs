{-# LANGUAGE OverloadedStrings #-}
module Service.Transaction.SproutCommon where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import qualified Data.HashTable.IO                     as H
import qualified Data.Serialize                        as S (decode, encode)
import           Service.InfoMsg                       (InfoMsg (..))
import           Service.Transaction.Storage
import           Service.Types


data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]
type Number = Integer
type From = Number
type To = Number
type Limit = Integer

data Common = Common {
  db       :: DBPoolDescriptor,
  infoChan :: InChan InfoMsg
 }

type MainChain = HashOfKeyBlock
type SproutChain = HashOfKeyBlock
type Chain = (Maybe MainChain, Maybe SproutChain)
type FullChain = (Integer, Maybe MainChain, Maybe SproutChain)

type SproutTable = H.BasicHashTable Number Chain


data SproutException = ValueOfChainIsNotNothing String
                  | NotImplementedException -- test
                  | OtherException
  deriving Show
instance Exception SproutException


lastKeyBlock :: DBKey
lastKeyBlock = "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4=" :: DBKey


writeKeyBlockNumber :: Common -> Number -> IO ()
writeKeyBlockNumber (Common descr _) aNumber= do
  let value = S.encode aNumber
  funW (poolSprout descr) [(lastKeyBlock, value)]


getKeyBlockNumber :: Common -> IO (Maybe Number)
getKeyBlockNumber (Common descr _) = do
  value <- funR (poolSprout descr) lastKeyBlock
  case value of
    Nothing -> return Nothing
    Just v  -> case S.decode v :: Either String Number of
      Left e  -> throw (DecodeException (show e))
      Right r -> return $ Just r
