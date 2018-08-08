{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.Transformation where
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import           Control.Monad                         (forM)
import qualified Data.ByteString                       as B
-- import           Node.Data.GlobalLoging
import           Service.InfoMsg                       (InfoMsg (..))
import           Service.Transaction.Decode
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeInstances      (roll, unroll)


-- Helper functions
getTeamKeysForMicroblock :: DBPoolDescriptor -> InChan InfoMsg -> HashOfKeyBlock -> IO [PublicKey]
getTeamKeysForMicroblock db i aHash = do
  mb <- getKeyBlockByHash db i (Hash aHash)
  case mb of Nothing -> do
               -- writeLog aInfoChan [BDTag] Error ("No Team Keys For Key block " ++ show aHash)
               return []
             Just r -> return $ _teamKeys (r :: MacroblockBD)


getTxsMicroblock :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO [Transaction]
getTxsMicroblock db i mb = do
  txDecoded <- getTxs db i mb
  return $ map (\t -> _tx  (t :: TransactionInfo)) txDecoded


getTxs :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO [TransactionInfo]
getTxs descr i mb = do
  let txHashes = _transactionsHashes mb
  forM txHashes $ getTx1 descr i
  where getTx1 :: DBPoolDescriptor -> InChan InfoMsg -> HashOfTransaction -> IO TransactionInfo
        getTx1 d _ h = do
          maybeTx <- getTransactionByHashDB d (Hash h)
          case maybeTx of
            Nothing -> throw $ NoSuchTransactionForHash ("hash: " ++ show h)
            Just j  -> return j
-------------------------



tMicroblockBD2MicroblockAPI :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO MicroblockAPI
tMicroblockBD2MicroblockAPI db i m@MicroblockBD {..} = do
  tx <- getTxsMicroblock db i m
  let txAPI = map (\t -> TransactionAPI {_tx = t, _txHash = rHashT t }) tx
  return MicroblockAPI {
            _prevMicroblock = Nothing,
            _nextMicroblock = Nothing,
            _keyBlock,
            _signAPI = _signBD,
            -- _teamKeys = teamKeys,
            _publisher,
            _transactionsAPI = txAPI
            }


tMicroblockBD2Microblock :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO Microblock
tMicroblockBD2Microblock db i m@MicroblockBD {..} = do
  tx <- getTxsMicroblock db i m
  aTeamkeys <- getTeamKeysForMicroblock db i _keyBlock
  return Microblock {
  _keyBlock,
  _sign          = _signBD,
  -- _teamKeys,
  _teamKeys = aTeamkeys,
  _publisher,
  _transactions  = tx
  -- _numOfBlock
  }


tKeyBlockToPoWType :: KeyBlockInfo -> KeyBlockInfoPoW
tKeyBlockToPoWType (KeyBlockInfo {..}) = KeyBlockInfoPoW{
  _time,
  _prev_hash,
  _number,
  _nonce,
  _solver = pubKey,
  _type}
  where pubKey = B.pack $ unroll $ fromPublicKey256k1 _solver


tKBIPoW2KBI :: KeyBlockInfoPoW -> KeyBlockInfo
tKBIPoW2KBI KeyBlockInfoPoW {..} = KeyBlockInfo {
  _time,
  _prev_hash,
  _number,
  _nonce,
  _solver = pubKey,
  _type}
  where pubKey = publicKey256k1 ((roll $ B.unpack _solver) :: Integer)


fillMacroblockByKeyBlock :: MacroblockBD -> KeyBlockInfo -> MacroblockBD
fillMacroblockByKeyBlock m KeyBlockInfo {..} = m {
        _prevHKBlock = Just _prev_hash,
        _solver = _solver,
        _time = _time,
        _number = _number,
        _nonce  = _nonce}


tMicroblock2MicroblockBD :: Microblock -> MicroblockBD
tMicroblock2MicroblockBD Microblock {..} = MicroblockBD {
  _keyBlock,
  _signBD = _sign,
  -- _teamKeys,
  _publisher,
  _transactionsHashes = map rHashT _transactions
  -- _numOfBlock = 0
  }


tKeyBlockInfo2Macroblock :: KeyBlockInfo -> MacroblockBD
tKeyBlockInfo2Macroblock KeyBlockInfo {..} = MacroblockBD {
            _prevKBlock = Nothing,
            _nextKBlock = Nothing,
            _prevHKBlock = Just _prev_hash,
            _difficulty = 20,
            _solver,
            _reward = 0,
            _time,
            _number,
            _nonce,
            _mblocks = [],
            _teamKeys = []
          }


tMacroblock2KeyBlockInfo :: MacroblockBD -> KeyBlockInfo
tMacroblock2KeyBlockInfo MacroblockBD {..} = KeyBlockInfo {
  _time     ,
  _prev_hash = prev_hash,
  _number   ,
  _nonce    ,
  _solver   ,
  _type = 0}
  where prev_hash = case _prevHKBlock of
          Nothing -> ""
          Just j  -> j


tMacroblock2ChainInfo :: Maybe (DBKey, MacroblockBD) -> IO ChainInfo
tMacroblock2ChainInfo kv = case kv of
    Nothing ->  return ChainInfo {
    _emission        = 0,
    _curr_difficulty = 0,
    _last_block      = "",
    _blocks_num      = 0,
    _txs_num         = 0,  -- quantity of all approved transactions
    _nodes_num       = 0   -- quantity of all active nodes
    }
    Just (aKeyBlockHash, MacroblockBD {..})  -> return ChainInfo {
    _emission        = _reward,
    _curr_difficulty = _difficulty,
    _last_block      = aKeyBlockHash,
    _blocks_num      = 0,
    _txs_num         = 0,  -- quantity of all approved transactions
    _nodes_num       = 0   -- quantity of all active nodes
    }


dummyMacroblock :: MacroblockBD
dummyMacroblock = MacroblockBD {
  _prevKBlock = Nothing,
  _nextKBlock = Nothing,
  _prevHKBlock = Nothing,
  _difficulty = 0,
  _solver = aSolver,
  _reward = 0,
  _time = 0,
  _number = 0,
  _nonce = 0,
  _mblocks = [],
  _teamKeys = []
}
  where aSolver = read "1" :: PublicKey


tMacroblock2MacroblockAPI :: DBPoolDescriptor -> MacroblockBD -> IO MacroblockAPI
tMacroblock2MacroblockAPI descr MacroblockBD {..} = do
           microblocks <- zip _mblocks <$> mapM (getMicroBlockByHashDB descr . Hash) _mblocks
           let microblocksInfoAPI = map (\(h, MicroblockBD {..}) -> MicroblockInfoAPI {
                                                        _prevMicroblock = Nothing,
                                                        _nextMicroblock = Nothing,
                                                        _keyBlock,
                                                        _signAPI = _signBD,
                                                        _publisher,
                                                        _hash = h}) microblocks
           return MacroblockAPI {
             _prevKBlock,
             _nextKBlock = Nothing,
             _difficulty,
             _height = _number,
             _solver,
             _reward,
             _mblocks = microblocksInfoAPI,
             _teamKeys }


decodeTx :: [DBValue] -> IO [TransactionAPI]
decodeTx txInfo = do
  let fun1 t = _tx (decodeThis "TransactionInfo" t :: TransactionInfo)
  let fun2 t = TransactionAPI { _tx = t, _txHash = rHashT t}
  return $ map (fun2 . fun1) txInfo
