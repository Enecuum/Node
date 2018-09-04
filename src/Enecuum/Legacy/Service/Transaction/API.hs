{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Enecuum.Legacy.Service.Transaction.API where

import           Enecuum.Legacy.Service.Transaction.Decode
import           Enecuum.Legacy.Service.Transaction.Iterator
import           Enecuum.Legacy.Service.Types
import           Enecuum.Legacy.Service.Types.PublicPrivateKeyPair
import           Enecuum.Legacy.Service.Types.SerializeJSON        ()
import           Universum


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
