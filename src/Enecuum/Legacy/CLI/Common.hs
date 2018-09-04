{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Enecuum.Legacy.CLI.Common (
  sendMessageTo,
  sendMessageBroadcast,
  loadMessages,

  sendTrans,
  sendNewTrans,
  getNewKey,
  getBlockByHash,
  getKeyBlockByHash,
  getTransactionByHash,
  getAllTransactionsByWallet,
  getPartTransactions,
  getBalance,
  getChainInfo,
  getPublicKeys,

  getAllChain,
  getAllLedger,
  getAllMicroblocks,
  getAllKblocks,
  getAllTransactions,

  Result

  )where

import           Control.Concurrent                                (threadDelay)
import           Control.Concurrent.Chan.Unagi.Bounded             (InChan,
                                                                    tryWriteChan)
-- import           Control.Concurrent.MVar                           (newEmptyMVar,
--                                                                     readMVar)
import           Control.Exception                                 (throw)
import           Control.Monad                                     (unless)
import           Control.Timeout                                   (timeout)
import           Data.List.Split                                   (splitOn)
import           Data.Map                                          (Map,
                                                                    fromList,
                                                                    lookup)
import           Data.Time.Units                                   (Millisecond,
                                                                    Second,
                                                                    getCPUTimeWithUnit,
                                                                    subTime)
import           System.Random                                     (randomRIO)

import           Data.Text                                         (pack,
                                                                    unpack)
import           Enecuum.Legacy.Node.Crypto                        (verifyEncodeble)
import           Enecuum.Legacy.Node.Node.Types                    (MsgToCentralActor (..))
import qualified Enecuum.Legacy.Service.InfoMsg                    as I
import           Enecuum.Legacy.Service.System.Directory           (getKeyFilePath,
                                                                    getTime)
import qualified Enecuum.Legacy.Service.Transaction.Common         as B
import           Enecuum.Legacy.Service.Types                      (CLIException (..),
                                                                    ChainInfo,
                                                                    Common (..),
                                                                    Currency (..),
                                                                    DBKey,
                                                                    DBPoolDescriptor,
                                                                    FullChain,
                                                                    Hash (..),
                                                                    InContainerChan,
                                                                    InfoMsg (..),
                                                                    MacroblockAPI,
                                                                    MacroblockBD,
                                                                    MicroblockAPI,
                                                                    MicroblockBD,
                                                                    MsgTo,
                                                                    Trans (..),
                                                                    Transaction (..),
                                                                    TransactionAPI,
                                                                    TransactionInfo)
import           Enecuum.Legacy.Service.Types.PublicPrivateKeyPair (Amount,
                                                                    KeyPair (..),
                                                                    PrivateKey,
                                                                    PublicKey,
                                                                    generateNewRandomAnonymousKeyPair,
                                                                    getPublicKey,
                                                                    getSignature,
                                                                    uncompressPublicKey)
import           Prelude                                           (head, read,
                                                                    (!!))
import           Universum                                         hiding (head)

type Result a = Either CLIException a

getAllChain :: Common -> IO (Result [FullChain])
getAllChain c = try $ B.getAllSproutKV c

getAllLedger :: Common -> IO (Result [(PublicKey, Amount)])
getAllLedger c = try $ B.getAllLedgerKV c

getAllMicroblocks :: Common -> IO (Result [(DBKey, MicroblockBD)])
getAllMicroblocks c =  try $ B.getAllMicroblockKV c

getAllKblocks :: Common -> IO (Result [(DBKey, MacroblockBD)])
getAllKblocks c =  try $ B.getAllMacroblockKV c

getAllTransactions :: Common -> IO (Result [(DBKey, TransactionInfo)])
getAllTransactions c =  try $ B.getAllTransactionsKV c




sendMessageTo :: MsgTo -> InChan MsgToCentralActor -> IO (Result ())
sendMessageTo _ _ = return $ return undefined


sendMessageBroadcast :: String -> InChan MsgToCentralActor -> IO (Result ())
sendMessageBroadcast _ = return $ return $ Left NotImplementedException


loadMessages :: InChan MsgToCentralActor -> IO (Result [MsgTo])
loadMessages _ = return $ Left NotImplementedException


getBlockByHash :: Common -> Hash -> IO (Result MicroblockAPI)
getBlockByHash c hash  = try $ do
  mb <- B.getBlockByHashDB c hash
  case mb of
    Nothing -> throw NoSuchMicroBlockDB
    Just m  -> return m


getKeyBlockByHash :: Common -> Hash -> IO (Result MacroblockAPI)
getKeyBlockByHash common (Hash h)  = try $ do
  mb <- B.getKeyBlockByHashDB common (Hash h)
  case mb of
    Nothing -> throw NoSuchMacroBlockDB
    Just m  -> return m


getChainInfo :: Common -> IO (Result ChainInfo)
getChainInfo c = do
  Right <$> B.getChainInfoDB c



getTransactionByHash :: Common -> Hash  -> IO (Result TransactionInfo)
getTransactionByHash (Common db _) hash = try $ do
  tx <- B.getTransactionByHashDB db hash
  case tx of
    Nothing -> throw NoSuchTransactionDB
    Just t  -> return t


getAllTransactionsByWallet :: Common -> PublicKey -> IO (Result [TransactionAPI])
getAllTransactionsByWallet c key = try $ do
  tx <- B.getAllTransactionsDB c key
  case tx of
    [] -> throw NoTransactionsForPublicKey
    t  -> return t


getPartTransactions :: Common -> InContainerChan -> PublicKey -> Int -> Int -> IO (Result [TransactionAPI])
getPartTransactions c inContainerChan key offset aCount = try $ do
  tx <- B.getLastTransactions c inContainerChan key offset aCount
  case tx of
    [] -> throw NoTransactionsForPublicKey
    t  -> return t


sendTrans :: Transaction -> InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result Hash)
sendTrans tx ch aInfoCh = try $ do
  aExp <- (timeout (5 :: Second) $ do
           case _signature tx of
            Just sign ->
                 if verifyEncodeble pk sign (tx {_signature = Nothing})
                 then do
                   sendMetrics tx aInfoCh
                   aMVar <- newEmptyMVar
                   cTime <- getTime
                   writeInChan ch $ NewTransaction (tx { _timestamp = Just cTime } ) aMVar
                   r <- readMVar aMVar
                   print r
                   case r of
                     True -> return $ B.rHash tx
                     _    -> throw TransactionChanBusyException
                 else
                   throw TransactionInvalidSignatureException
            _         -> throw TransactionInvalidSignatureException
          )
  case aExp of
    Just h  -> return $ Hash h
    Nothing -> throw TransactionChanBusyException
 where pk = getPublicKey $ uncompressPublicKey $ _owner tx


sendNewTrans :: Trans -> InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result Hash)
sendNewTrans aTrans ch aInfoCh = do
  let moneyAmount = txAmount aTrans :: Amount
  let receiverPubKey = recipientPubKey aTrans
  let ownerPubKey = senderPubKey aTrans
  keyPairs <- getSavedKeyPairs
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  case Data.Map.lookup ownerPubKey mapPubPriv of
    Nothing -> throw WrongKeyOwnerException
    Just ownerPrivKey -> do
      uuid <- randomRIO (1,25)
      let tx  = Transaction ownerPubKey receiverPubKey moneyAmount ENQ Nothing Nothing uuid
      sign <- getSignature ownerPrivKey tx
      let signTx  = tx { _signature = Just sign }
      sendTrans signTx ch aInfoCh



getNewKey :: IO (Result PublicKey)
getNewKey = try $ do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (pack $ show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
  putStrLn ("Public Key " ++ show aPublicKey ++ " was created")
  return aPublicKey


getBalance :: DBPoolDescriptor -> PublicKey -> InChan InfoMsg -> IO (Result Amount)
getBalance descrDB pKey aInfoCh = try $ do
    stTime  <- getCPUTimeWithUnit :: IO Millisecond
    aBalance <- B.getBalanceForKey descrDB pKey
    endTime <- getCPUTimeWithUnit :: IO Millisecond
    writeInChan aInfoCh $ Metric $ I.timing "cl.ld.time" (subTime stTime endTime)
    case aBalance of
      Nothing -> throw NoSuchPublicKeyInDB
      Just b  -> return b


getSavedKeyPairs :: IO [(PublicKey, PrivateKey)]
getSavedKeyPairs = do
  result <- try $ getKeyFilePath >>= readFile
  case result of
    Left ( _ :: SomeException) -> do
          print "There is no keys"
          return []
    Right keyFileContent       -> do
          let rawKeys = lines keyFileContent
          let keys = map (splitOn ":" . unpack) rawKeys
          let pairs = map (\x -> (,) (read (head x) :: PublicKey) (read (x !! 1) :: PrivateKey)) keys
          return pairs


getPublicKeys :: IO (Result [PublicKey])
getPublicKeys = try $ map fst <$> getSavedKeyPairs


sendMetrics :: Transaction -> InChan InfoMsg -> IO ()
sendMetrics (Transaction o r a _ _ _ _) m = do
    writeInChan m $ Metric $ I.increment "cl.tx.count"
    writeInChan m $ Metric $ I.set "cl.tx.wallet" o
    writeInChan m $ Metric $ I.set "cl.tx.wallet" r
    writeInChan m $ Metric $ I.gauge "cl.tx.amount" a


-- safe way to write in chan
writeInChan :: InChan t -> t -> IO ()
writeInChan aChan aMsg = do
    aOk <- tryWriteChan aChan aMsg
    threadDelay 10000
    unless aOk $ writeInChan aChan aMsg
