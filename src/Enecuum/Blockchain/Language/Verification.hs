{-# LANGUAGE RecordWildCards #-}

module Enecuum.Blockchain.Language.Verification where

import qualified Enecuum.Blockchain.Domain  as D
import qualified Enecuum.Blockchain.Lens    as Lens
import           Enecuum.Core.Crypto.Crypto
import           Enecuum.Prelude

type BlockValid = Bool
type TransactionValid = Bool

verifyMicroblock :: D.Microblock -> BlockValid
verifyMicroblock mb@D.Microblock {..} = verifyEncodable _publisher _signature (D.microblockForSign mb)

verifyTransaction :: D.Transaction -> TransactionValid
verifyTransaction t@D.Transaction {..} = verifyEncodable _owner _signature (D.transactionForSign t)
