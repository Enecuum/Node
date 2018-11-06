{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Blockchain.Language.Ledger where

import           Data.Map
import qualified Data.Map                                 as Map
import qualified Enecuum.Blockchain.Domain                as D
import           Enecuum.Blockchain.Domain.BlockchainData (BlockchainData (..))
import           Enecuum.Blockchain.Domain.Microblock     (Microblock (..))
import           Enecuum.Blockchain.Domain.Transaction    (Transaction (..))
import qualified Enecuum.Core.Types                       as D
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Domain                 as D
import qualified Enecuum.Framework.Language               as L
import           Enecuum.Prelude

newWalletAmount :: D.Amount
newWalletAmount = 100

initializeWallet :: D.StateVar D.Ledger -> D.WalletID -> L.StateL ()
initializeWallet ledgerVar wallet = do
    ledgerW <- L.readVar ledgerVar
    unless (Map.member wallet ledgerW) $ L.modifyVar ledgerVar (Map.insert wallet newWalletAmount)

getBalanceOrCrash :: D.WalletID -> D.Ledger -> D.Amount
getBalanceOrCrash wallet ledger = fromMaybe
    (error $ "Impossible: wallet " +|| wallet ||+ " is not initialized.")
    (ledger ^. at wallet)

calculateLedger :: BlockchainData -> Microblock -> L.StateL ()
calculateLedger bData mblock =

    forM_ (_transactions mblock) $ \tx -> do
        let ledgerVar = _ledger bData

        initializeWallet ledgerVar $ _owner    tx
        initializeWallet ledgerVar $ _receiver tx

        ledgerW <- L.readVar ledgerVar

        let owner              = _owner tx
        let receiver           = _receiver tx
        let amount             = _amount tx
        let ownerBalance       = getBalanceOrCrash owner ledgerW
        let receiverBalance    = getBalanceOrCrash receiver ledgerW
        let newOwnerBalance    = ownerBalance - amount
        let newReceiverBalance = receiverBalance + amount

        let newLedger = insert owner
                               newOwnerBalance
                               (insert receiver newReceiverBalance ledgerW)
        let transactionValid = owner /= receiver && ownerBalance >= amount

        when transactionValid    $ L.writeVar ledgerVar newLedger

        when transactionValid    $ L.logInfo $ "Tx accepted: " +|| D.showTx tx newOwnerBalance newReceiverBalance ||+ "."
        when (owner == receiver) $ L.logInfo $ "Tx rejected (same owner and receiver): " +| D.showPublicKey owner |+ "."
        unless transactionValid  $ L.logInfo $ "Tx rejected (negative balance): " +|| D.showTx tx newOwnerBalance newReceiverBalance ||+ "."
