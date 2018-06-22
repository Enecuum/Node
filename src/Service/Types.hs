{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, LambdaCase, StandaloneDeriving #-}
{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
module Service.Types where

import              Data.Serialize
import              Data.Graph.Inductive
import              Service.Types.PublicPrivateKeyPair
import              GHC.Generics
import              Data.ByteString
import              Data.List.Split (splitOn)
import qualified    Data.ByteString.Char8 as C


type QuantityTx = Int
data Trans = Trans {
        txAmount :: Amount
      , recipientPubKey :: PublicKey
      , senderPubKey :: PublicKey
      , currency :: Currency
      } deriving (Eq, Show, Generic)

type Id = Integer
data MsgTo = MsgTo {
        messageTo      :: Id
      , messageContent :: String
      } deriving (Eq, Show, Generic)

instance Read Trans where
    readsPrec _ value =
        case splitOn ":" value of
             [f1, f2, f3, f4] ->
                 [(Trans (read f1) (read f2) (read f3) (read f4), [])]
             x -> error $ "Invalid number of fields in input: " ++ show x


instance Read MsgTo where
     readsPrec _ value =
        case splitOn ":" value of
             [t, m] ->  [(MsgTo (read t) m, [])]
             x      -> error $ "Invalid number of fields in input: " ++ show x

data Currency = ENQ | ETH | DASH | BTC deriving (Ord,Eq,Read,Show,Generic)
instance Serialize Currency

type Time      = Int -- UnixTimestamp
type DAG = Gr Transaction Transaction

newtype Hash = Hash ByteString deriving (Ord, Eq, Show, Generic)
instance Serialize Hash

instance Read Hash where
       readsPrec _ value = return (Hash $ C.pack value,"")

data MicroblockV1 = MicroblockV1{
                  hashCurrentMicroblock :: ByteString, -- hashCurrentMicroblock
                  hashPreviousMicroblock :: ByteString, -- hashPreviousMicroblock
                  trans :: [Transaction]}
                deriving (Eq, Generic, Ord, Show)

data Microblock = Microblock{
    _keyBlock :: ByteString, -- hash of key-block
    _signer :: PublicKey,
    _sign :: Signature,  -- signature for {K_hash, [Tx],}
    _teamKeys :: [PublicKey], -- for reward
    _transactions :: [Transaction],
    _numOfBlock   :: Integer
  }
  deriving (Eq, Generic, Ord, Read)

instance Serialize Microblock
instance Show Microblock where
    show _ = "Microblock ??"

data Macroblock = Macroblock {
    _prevBlock :: ByteString
  ,  _difficulty :: Integer
  ,  _height :: Integer
  ,  _solver :: ByteString
  ,  _reward :: Integer
  ,  _txs_cnt :: Integer
  ,  _mblocks :: [ByteString]
} deriving (Eq, Generic, Ord, Read, Show)
instance Serialize Macroblock


data Transaction = Transaction {
  _owner     :: PublicKey,
  _receiver  :: PublicKey,
  _amount    :: Amount,
  _currency  :: Currency,
  _time      :: Time, -- UnixTime format
  _signature :: Signature
}  deriving ( Generic, Show, Eq, Ord, Read)


instance Serialize Transaction

data TransactionInfo = TransactionInfo {
     _tx    :: Transaction
  ,  _block :: ByteString
  ,  _index :: Int
  } deriving (Generic, Show, Eq, Read)
instance Serialize TransactionInfo

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


type ToPublicKey  = PublicKey
data MessageForSign = MessageForSign ToPublicKey Amount Time
instance Serialize MessageForSign
deriving instance Generic MessageForSign


data ChainInfo = ChainInfo {
      _emission        :: Integer
    , _curr_difficulty :: Integer
    , _blocks_num      :: Integer
    , _txs_num         :: Integer
    , _nodes_num       :: Integer
  } deriving  (Generic, Show, Eq, Read)
instance Serialize ChainInfo
