{-# LANGUAGE RecordWildCards #-}

module Enecuum.Blockchain.Language.Verification where

import           Enecuum.Prelude

import qualified Enecuum.Blockchain.Domain as D
import qualified Enecuum.Blockchain.Lens as Lens
import           Enecuum.Core.Crypto.Crypto


type Valid = Bool
type BlockValid = Bool
type TransactionValid = Bool


verifyMicroblock :: D.Microblock -> Bool
verifyMicroblock mb@D.Microblock {..} = verifyEncodable _publisher _signature (D.microblockForSign mb)

verifyTransaction :: D.Transaction -> Bool
verifyTransaction t@D.Transaction {..} = verifyEncodable _owner _signature (D.transactionForSign t)

verifyMicroblockWithTx :: D.Microblock -> (Valid, BlockValid, [TransactionValid])
verifyMicroblockWithTx mBlock = (valid, blockValid, txsValid)
    where
        blockValid = verifyMicroblock mBlock 
        txsValid   = map verifyTransaction $ mBlock ^. Lens.transactions
        valid      = blockValid && and txsValid

-- verifyMicroblockWithTxEff :: (Monad m, L.Logger m) => Microblock -> m Bool
-- verifyMicroblockWithTxEff mBlock = do
--     let isSignMbGenuine = verifyMicroblock mBlock
--         tx = _transactions (mBlock :: Microblock)
--         bogusTx = filter (\a -> snd a == False ) $ zip tx (map verifyTransaction tx)
--     case isSignMbGenuine of
--         True -> L.logInfo $ "Signature of microblock is genuine."
--         False -> L.logInfo $ "Signature of microblock is not genuine. Someone is trying to pretend to be publisher " +|| (show $ showPublicKey $ _publisher (mBlock :: Microblock) )
--     forM_ bogusTx $ \tx -> L.logInfo $ "Signature of transaction "  +|| fst tx ||+ " is not genuine "
--     pure $ isSignMbGenuine && (null bogusTx)
