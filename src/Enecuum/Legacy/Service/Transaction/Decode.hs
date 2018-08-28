{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Decode where
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import qualified Crypto.Hash.SHA256                    as SHA
import           Data.Aeson                            hiding (Error)
import           Data.Aeson.Types                      (parseMaybe)
import qualified Data.ByteString.Base64                as Base64
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Internal              as BSI
import           Data.Default                          (def)
import           Data.Pool
import qualified Data.Serialize                        as S (Serialize (..),
                                                             decode, encode)
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()


-- for rocksdb Transaction and Microblock
rHashT :: Transaction -> BSI.ByteString
rHashT t@Transaction {} = Base64.encode . SHA.hash . S.encode $ t { _timestamp = Nothing }

rHash :: S.Serialize a => a -> BSI.ByteString
rHash key = Base64.encode . SHA.hash . S.encode $ key


lastKeyBlock :: DBKey
lastKeyBlock = "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4=" :: DBKey


funW ::  Pool Rocks.DB -> [(DBKey, DBValue)] -> IO ()
funW db aMapKeyValue = do
  let fun aDb =  Rocks.write aDb def{Rocks.sync = True} $ map (uncurry Rocks.Put) aMapKeyValue
  withResource db fun


funR ::  Pool Rocks.DB -> DBKey -> IO (Maybe BSI.ByteString)
funR db key = do
  let fun aDb = Rocks.get aDb Rocks.defaultReadOptions key
  withResource db fun


funD ::  Pool Rocks.DB -> DBKey -> IO ()
funD db key = do
  let fun aDb = Rocks.delete aDb def{Rocks.sync = True} key
  withResource db fun


getByHash :: Pool Rocks.DB -> Hash -> IO (Maybe DBValue)
getByHash pool aHash = (\(Hash key) -> funR pool key) aHash


type DecodeType = String

---- Decode
decodeThis :: S.Serialize p => DecodeType -> BSI.ByteString ->  p
decodeThis aType res = case S.decode res of
  Left e  -> throw $ DecodeException $ aType ++ show e
  Right r -> r


decodeRaw :: S.Serialize a => DecodeType -> Maybe DBValue ->  Maybe a
decodeRaw aType this = case this of
  Nothing -> Nothing
  Just j  -> Just $ decodeThis aType j


-- MacroblockBD
getKeyBlockByHash :: Common -> Hash  -> IO (Maybe MacroblockBD)
getKeyBlockByHash (Common db _) kHash = decodeRaw "MacroblockBD" <$> getByHash (poolMacroblock db) kHash


--Microblock
getMicroBlockByHashDB :: Common -> Hash -> IO MicroblockBD
getMicroBlockByHashDB (Common db _) mHash = do
  res <- decodeRaw "MicroblockBD" <$> getByHash (poolMicroblock db) mHash
  case res of
    Nothing -> throw (NoSuchMicroBlockForHash $ show mHash)
    Just j  -> return j


--Transaction
getTransactionByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe TransactionInfo)
getTransactionByHashDB db tHash = decodeRaw "TransactionInfo" <$> getByHash (poolTransaction db) tHash


decodeTransactionAndFilterByKey :: PublicKey -> DBValue ->  Maybe TransactionAPI
decodeTransactionAndFilterByKey pubKey rawTx  = txAPI
  where txInfo = decodeThis "TransactionInfo" rawTx :: TransactionInfo
        tx = _tx (txInfo :: TransactionInfo)
        txAPI = if txFilterByKey pubKey tx
          then Just TransactionAPI { _tx = tx, _txHash = rHashT tx}
          else Nothing


decodeAndFilter :: PublicKey -> DBValue -> Bool
decodeAndFilter pubKey rawTx  = isKeyThere
  where txInfo = decodeThis "TransactionInfo" rawTx :: TransactionInfo
        tx = _tx (txInfo :: TransactionInfo)
        isKeyThere = txFilterByKey pubKey tx


txFilterByKey :: PublicKey -> Transaction -> Bool
txFilterByKey pubKey t = _owner t == pubKey || _receiver t == pubKey


-- Chain
getChain :: Common -> Number -> IO Chain
getChain (Common descr _ ) aNumber = do
  maybeV <- funR (poolSprout descr) (S.encode aNumber)
  case maybeV of
    Nothing -> return (Nothing, Nothing)
    Just m  -> return $ decodeThis "Chain" m


--Ledger
getBalanceForKey :: DBPoolDescriptor -> PublicKey -> IO (Maybe Amount)
getBalanceForKey db key = decodeRaw "Amount" <$> funR (poolLedger db) (S.encode key)


--Last Number
getLastKeyBlockNumber :: Common -> IO (Maybe Number)
getLastKeyBlockNumber (Common descr _) = decodeRaw "Number" <$> funR (poolLast descr) lastKeyBlock


--KeyBlock
decodeKeyBlock :: InChan InfoMsg -> Value -> IO KeyBlockInfoPoW
decodeKeyBlock i (Object aValue) = do
  let keyBlock = case parseMaybe (.: "verb") aValue of
        Nothing     -> throw (DecodeException "There is no verb in PoW Key Block")
        Just kBlock -> kBlock :: BC.ByteString 
  if keyBlock /= "kblock"
    then throw $ DecodeException $ "Expected kblock, but get: " ++ show keyBlock
    else do
    let body = case parseMaybe (.: "body") aValue of
          Nothing     -> throw (DecodeException "Can not parse body of PoW Key Block ")
          Just kBlock -> kBlock :: BC.ByteString

    case Base64.decode body of
      Left e -> throw (DecodeException (show e))
      Right r -> case Data.Aeson.eitherDecodeStrict $ BC.init $ BC.tail r of
          Left a -> throw (DecodeException $ "There is no PoW Key Block. The error: " ++ a)
          Right keyBlockInfo -> do
            writeLog i [KeyBlockTag] Info $ "keyBlockInfo: " ++ show keyBlockInfo
            return keyBlockInfo
decodeKeyBlock _ v  = throw $ DecodeException $ "Can not decode PoW Key Block" ++ show v
