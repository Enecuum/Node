{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Service.Types where

import           Data.ByteString
import qualified Data.ByteString.Char8              as C
import           Data.Graph.Inductive
import           Data.List.Split                    (splitOn)
import           Data.Serialize
import           GHC.Generics
import           Service.Types.PublicPrivateKeyPair

type QuantityTx = Int
data Trans = Trans {
        txAmount        :: Amount
      , recipientPubKey :: PublicKey
      , senderPubKey    :: PublicKey
      , currency        :: Currency
      } deriving (Eq, Show, Generic)

type Id = Integer
data MsgTo = MsgTo {
        messageTo      :: Id
      , messageContent :: String
      } deriving (Eq, Show, Generic, Ord)

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


data PartWalletReq = PartWalletReq {
    _key    :: PublicKey
  , _offset :: Integer
  , _count  :: Integer
  } deriving (Eq, Show, Ord)

instance Read PartWalletReq where
    readsPrec _ value =
        case splitOn ":" value of
             [f1, f2, f3] ->
                 [(PartWalletReq (read f1) (read f2) (read f3), [])]
             x -> error $ "Invalid number of fields in input: " ++ show x



type Time      = Int -- UnixTimestamp
type DAG = Gr Transaction Transaction

newtype Hash = Hash ByteString deriving (Ord, Eq, Show, Generic)
instance Serialize Hash

instance Read Hash where
       readsPrec _ value = return (Hash $ C.pack value,"")

data MicroblockV1 = MicroblockV1{
                  hashCurrentMicroblock  :: ByteString, -- hashCurrentMicroblock
                  hashPreviousMicroblock :: ByteString, -- hashPreviousMicroblock
                  trans                  :: [Transaction]}
                deriving (Eq, Generic, Ord, Show)


-- MicroblockPoA
data Microblock = Microblock{
    _keyBlock     :: ByteString, -- hash of key-block
    _sign         :: Signature,  -- signature for {K_hash, [Tx],}
    _teamKeys     :: [PublicKey], -- for reward
    _transactions :: [Transaction],
    _numOfBlock   :: Integer
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Serialize Microblock

data MicroblockBD = MicroblockBD{
    _keyBlock       :: ByteString, -- hash of key-block
    _signBD         :: Signature,  -- signature for {K_hash, [Tx],}
    _teamKeys       :: [PublicKey], -- for reward
    _transactionsBD :: [ByteString], -- hashes of [Transaction],
    _numOfBlock     :: Integer
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Serialize MicroblockBD

data MicroblockAPI = MicroblockAPI {
     _prevMicroblock  :: ByteString  -- hash of the previous microblock if exists
    ,_nextMicroblock  :: ByteString  -- hash of the next microblock if exists
    ,_keyBlock        :: ByteString  -- hash of key-block
    ,_signAPI         :: Signature   -- signature for {K_hash, [Tx],}
    ,_teamKeys        :: [PublicKey] -- for reward
    ,_publisher       :: PublicKey
    ,_transactionsAPI :: [TransactionAPI]
  }
  deriving (Eq, Generic, Ord, Read, Show)
instance Serialize MicroblockAPI


data Macroblock = Macroblock {
     _prevKBlock :: ByteString
  ,  _difficulty :: Integer --
  ,  _height     :: Integer -- block number in the chain
  ,  _solver     :: PublicKey
  ,  _reward     :: Integer
  ,  _time       :: Integer
  ,  _number     :: Integer
  ,  _nonce      :: Integer
  ,  _mblocks    :: [ByteString]
  } deriving (Eq, Generic, Ord, Read, Show)
instance Serialize Macroblock

data MacroblockAPI = MacroblockAPI {
     _prevKBlock :: ByteString
  ,  _nextKBlock :: ByteString
  ,  _difficulty :: Integer
  ,  _height     :: Integer
  ,  _solver     :: PublicKey
  ,  _reward     :: Integer
  ,  _txsCnt     :: Integer
  ,  _mblocks    :: [ByteString]

} deriving (Eq, Generic, Ord, Read, Show)
instance Serialize MacroblockAPI

data KeyBlockInfo = KeyBlockInfo {
    _time     :: Integer
  , prev_hash :: ByteString --String
  , _number   :: Integer
  , _nonce    :: Integer
  , _solver   :: PublicKey
  } deriving (Eq, Generic, Ord, Read, Show)
instance Serialize KeyBlockInfo


data TransactionAPI = TransactionAPI {
    _tx     :: Transaction
  , _txHash :: ByteString  -- hash of Transaction
  } deriving (Generic, Show, Eq, Ord, Read)
instance Serialize TransactionAPI

data Transaction = Transaction {
  _owner     :: PublicKey,
  _receiver  :: PublicKey,
  _amount    :: Amount,
  _currency  :: Currency,
  _timestamp :: Maybe Time, -- UnixTime format
  _signature :: Maybe Signature,
  _uuid      :: Int
} deriving ( Generic, Show, Eq, Ord, Read)


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
      _emission        :: Integer     -- emission of last closed key block
    , _curr_difficulty :: Integer     -- difficulty of last closed key block
    , _last_block      :: ByteString  -- hash of last closed key block
    , _blocks_num      :: Integer     -- quantity of all mined blocks
    , _txs_num         :: Integer     -- quantity of all mined transactions
    , _nodes_num       :: Integer     -- quantity of all active nodes now
  } deriving  (Generic, Show, Eq, Read)
instance Serialize ChainInfo
