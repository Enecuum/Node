{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Service.Types where

import              Data.Serialize
import              Data.Graph.Inductive
import              Service.Types.PublicPrivateKeyPair
import              GHC.Generics
import              Data.ByteString
import qualified    Data.ByteString.Base16 as B16

data CryptoCurrency = ENQ | ETH | DASH | BTC deriving (Ord,Eq,Read,Show,Generic)

type Time      = Double
type DAG = Gr Transaction Transaction

data Microblock = Microblock ByteString ByteString [Transaction] deriving (Eq, Generic, Ord)
instance Serialize Microblock

instance Show Microblock where
    show (Microblock aByteString1 aByteString2 tr) =
        " hash1: " ++ show (B16.encode aByteString1) ++
        " hash2: " ++ show (B16.encode aByteString2) ++
        " transactions: " ++ show tr

data Transaction = WithTime { time :: Time, transaction :: Transaction }
                 | WithSignature { transaction :: Transaction, signature :: Signature }
                 | RegisterPublicKey { pubKey :: PublicKey, startWithBalance :: Amount }
                 | SendAmountFromKeyToKey { owner :: PublicKey, receiver :: PublicKey, amount :: Amount }
--                 | AccumulatorsToInductors { owner :: PublicKey, amount :: Amount }
--                 | InductorsToAccumulators { owner :: PublicKey, amount :: Amount }
--                 | SendInductorsFromKeyToKey { owner :: PublicKey, receiver :: PublicKey, amount :: Amount }
  deriving ( Generic, Show, Eq, Ord)


instance Serialize Transaction

data Ledger = Ledger { currentTime :: Time, ltable :: [LedgerEntry] }
  deriving (Show, Generic)

data LedgerEntry = LE { balanceFor :: PublicKey, startTime :: Time, history :: Either (LHistory INVALID) (LHistory VALID) }
  deriving (Show, Generic)

{-
data LHistory = Valid { valid :: Time, balance :: Double, prev :: LHistory }
              | Invalid { invalid :: Time, prev :: LHistory }
              | End
  deriving (Show)
-}

data VALID    deriving (Generic)
data INVALID  deriving (Generic)

data LHistory a where
      Invalid :: { invalid :: Time,                    history :: LHistory VALID } -> LHistory INVALID
      Valid   :: { valid   :: Time, balance :: Amount, prev    :: LHistory VALID } -> LHistory VALID
      End     ::                                                                      LHistory VALID


instance Show (LHistory INVALID) where
  show (Invalid tm hst) = "Invalid { invalid = " ++ show tm ++ ", history = " ++ show hst ++ " }"

instance Show (LHistory VALID) where
  show End = "End"
  show (Valid tm bl pr) = "Valid { valid = " ++ show tm ++ ", balance = " ++ show bl ++ ", prev = " ++ show pr ++ " }"
