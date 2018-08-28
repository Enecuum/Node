{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.API where

import           Service.Transaction.Decode
import           Service.Transaction.Iterator
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON        ()


getAllSproutKV :: Common -> IO [(Integer, (Maybe MainChain, Maybe SproutChain))]
getAllSproutKV (Common db _) = getAllAndDecode2 (poolSprout db) "Integer" "(Maybe MainChain, Maybe SproutChain)"


getAllLedgerKV :: Common -> IO [(PublicKey,Amount)]
getAllLedgerKV (Common db _) = getAllAndDecode2 (poolLedger db) "PublicKey" "Amount"


getAllTransactionsKV :: Common -> IO [(HashOfTransaction,TransactionInfo)]
getAllTransactionsKV (Common db _) = getAllAndDecode (poolTransaction db) id (decodeThis "TransactionInfo")


getAllMicroblockKV :: Common -> IO [(HashOfMicroblock,MicroblockBD)]
getAllMicroblockKV (Common db _) = getAllAndDecode (poolMicroblock db) id (decodeThis "MicroblockBD")


getAllMacroblockKV :: Common -> IO [(HashOfKeyBlock,MacroblockBD)]
getAllMacroblockKV (Common db _) = getAllAndDecode (poolMacroblock db) id (decodeThis "MacroblockBD")
