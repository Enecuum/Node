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

module Enecuum.Legacy.Service.Transaction.Transformation where
import           Control.Exception
import           Control.Monad                                     (forM)
import qualified Data.ByteString                                   as B
import           Enecuum.Legacy.Service.Transaction.Decode
import           Enecuum.Legacy.Service.Types
import           Enecuum.Legacy.Service.Types.PublicPrivateKeyPair
import           Enecuum.Legacy.Service.Types.SerializeInstances   (roll,
                                                                    unroll)
import           Enecuum.Prelude


-- Helper functions
getTeamKeysForMicroblock :: Common -> HashOfKeyBlock -> IO [PublicKey]
getTeamKeysForMicroblock (Common db i) aHash = do
  mb <- getKeyBlockByHash (Common db i) (Hash aHash)
  case mb of Nothing -> do
               return []
             Just r -> return $ _teamKeys (r :: MacroblockBD)


getTxsMicroblock :: Common -> MicroblockBD -> IO [Transaction]
getTxsMicroblock c mb = do
  txDecoded <- getTxs c mb
  return $ map (\t -> _tx  (t :: TransactionInfo)) txDecoded


getTxs :: Common -> MicroblockBD -> IO [TransactionInfo]
getTxs c mb = do
  let txHashes = _transactionsHashes mb
  forM txHashes $ getTx1 c
  where getTx1 :: Common -> HashOfTransaction -> IO TransactionInfo
        getTx1 (Common d _) h = do
          maybeTx <- getTransactionByHashDB d (Hash h)
          case maybeTx of
            Nothing -> throw $ NoSuchTransactionForHash ("hash: " ++ show h)
            Just j  -> return j



tMicroblockBD2MicroblockAPI :: Common -> MicroblockBD -> IO MicroblockAPI
tMicroblockBD2MicroblockAPI c m@MicroblockBD {..} = do
  tx <- getTxsMicroblock c m
  let txAPI = map (\t -> TransactionAPI {_tx = t, _txHash = rHashT t }) tx
  return MicroblockAPI {
            _prevMicroblock = Nothing,
            _nextMicroblock = Nothing,
            _keyBlock,
            _signAPI = _signBD,
            _publisher,
            _transactionsAPI = txAPI
            }


tMicroblockBD2Microblock :: Common -> MicroblockBD -> IO Microblock
tMicroblockBD2Microblock c m@MicroblockBD {..} = do
  tx <- getTxsMicroblock c m
  aTeamkeys <- getTeamKeysForMicroblock c _keyBlock
  return Microblock {
  _keyBlock,
  _sign          = _signBD,
  _teamKeys = aTeamkeys,
  _publisher,
  _transactions  = tx
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
  _publisher,
  _transactionsHashes = map rHashT _transactions
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


tMacroblock2MacroblockAPI :: Common -> MacroblockBD -> IO MacroblockAPI
tMacroblock2MacroblockAPI c MacroblockBD {..} = do
           microblocks <- zip _mblocks <$> mapM (getMicroBlockByHashDB c . Hash) _mblocks
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
