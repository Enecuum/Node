{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Blockchain.Language.Ledger where

import Enecuum.Prelude
import Data.Map
import qualified Enecuum.Framework.Language as L
import qualified Enecuum.Core.Language as L
import qualified Enecuum.Framework.Domain as D
import qualified Enecuum.Core.Types as D
import Enecuum.Blockchain.Domain.BlockchainData (BlockchainData(..))
import qualified Enecuum.Blockchain.Domain.Graph as D
import qualified Enecuum.Blockchain.Domain.KBlock as D
import  Enecuum.Blockchain.Domain.Microblock  (Microblock(..))
import  Enecuum.Blockchain.Domain.Transaction (Transaction(..))


import qualified Enecuum.Framework.LogState as Log

calculateLedger :: D.StateVar [Text] -> BlockchainData -> Microblock -> Free L.StateF ()
calculateLedger logV bData mblock = do
    forM_ (_transactions mblock) $ \tx -> do
        ledgerW <- L.readVar $ (_ledger bData)
        -- stateLog nodeData $ "Current Ledger " +|| ledgerW ||+ "."
        let owner          = _owner tx
            receiver       = _receiver tx
            amount         = _amount tx
            currentBalance = lookup owner ledgerW

        when (owner == receiver) $ Log.stateLog logV $ "Tx rejected (same owner and receiver): " +|| owner ||+ "."

        when (owner /= receiver) $ do
            ownerBalance <- case currentBalance of
                    Nothing           -> (Log.stateLog logV $ "Can't find wallet in ledger: " +|| owner ||+ ".") >> pure 100
                    Just ownerBalance -> pure ownerBalance 
            if ownerBalance >= amount 
            then do
-- stateLog nodeData $ "Before tx owner " +|| owner ||+ " has balance: " +|| balance ||+ "."
                let receiverBalance = fromMaybe 0 $ lookup receiver ledgerW
                -- stateLog nodeData $ "Before tx receiver " +|| receiver ||+ " has balance: " +|| receiverBalance ||+ "."
                let
                    newLedger = insert owner
                                       (ownerBalance - amount)
                                       (insert receiver (receiverBalance + amount) ledgerW)
                L.writeVar (_ledger bData) newLedger
                Log.stateLog logV
                    $   "Tx accepted: from [" +|| owner ||+ "] to [" +|| receiver ||+ "], amount: " +|| amount ||+ ". ["
                    +|| owner ||+ "]: " +|| ownerBalance - amount ||+ ", [" +|| receiver ||+ "]: " +|| receiverBalance + amount ||+ ""
-- stateLog nodeData $ "New Ledger " +|| newLedger ||+ "."
            else
                Log.stateLog logV
                $   "Tx rejected (negative balance): [" +|| owner ||+ "] -> [" +|| receiver ||+ "], amount: " +|| amount ||+ "."