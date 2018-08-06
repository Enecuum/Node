{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CLI.Common (
  sendMessageTo,
  sendMessageBroadcast,
  loadMessages,

  sendTrans,
  sendNewTrans,
  generateNTransactions,
  generateTransactionsForever,
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

  CLIException(..),
  Result

  )where

import           Control.Concurrent                    (threadDelay)
-- import           Control.Concurrent.Chan
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import           Control.Monad                         (forever, unless)
import           Data.List.Split                       (splitOn)
import           Data.Map                              (Map, fromList, lookup)
import           Data.Time.Units
import           System.Random                         (randomRIO)

import           Control.Concurrent.MVar
import           Node.Crypto                           (verifyEncodeble)
import           Node.Node.Types
import           Service.InfoMsg
import           Service.System.Directory              (getKeyFilePath, getTime)
import qualified Service.Transaction.Common            as B
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()

import           Control.Timeout
import           Data.Time.Units                       (Second)



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


getBlockByHash :: Common -> Hash  -> IO (Result MicroblockAPI)
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


--

getPartTransactions :: Common -> InContainerChan -> PublicKey -> Int -> Int -> IO (Result [TransactionAPI])
getPartTransactions c inContainerChan key offset aCount = try $ do --return $ Left NotImplementedException
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
  let moneyAmount = Service.Types.txAmount aTrans :: Amount
  let receiverPubKey = recipientPubKey aTrans
  let ownerPubKey = senderPubKey aTrans
  --timePoint <- getTime
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
  getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
  putStrLn ("Public Key " ++ show aPublicKey ++ " was created")
  return aPublicKey


getBalance :: DBPoolDescriptor -> PublicKey -> InChan InfoMsg -> IO (Result Amount)
getBalance descrDB pKey aInfoCh = try $ do
    stTime  <- getCPUTimeWithUnit :: IO Millisecond
    aBalance <- B.getBalanceForKey descrDB pKey
    endTime <- getCPUTimeWithUnit :: IO Millisecond
    writeInChan aInfoCh $ Metric $ timing "cl.ld.time" (subTime stTime endTime)
    case aBalance of
      Nothing -> throw NoSuchPublicKeyInDB
      Just b  -> return b
    --putStrLn "There is no such key in database"
    -- return result


getSavedKeyPairs :: IO [(PublicKey, PrivateKey)]
getSavedKeyPairs = do
  result <- try $ getKeyFilePath >>= readFile
  case result of
    Left ( _ :: SomeException) -> do
          putStrLn "There is no keys"
          return []
    Right keyFileContent       -> do
          let rawKeys = lines keyFileContent
          let keys = map (splitOn ":") rawKeys
          let pairs = map (\x -> (,) (read (head x) :: PublicKey) (read (x !! 1) :: PrivateKey)) keys
          return pairs


getPublicKeys :: IO (Result [PublicKey])
getPublicKeys = try $ map fst <$> getSavedKeyPairs


sendMetrics :: Transaction -> InChan InfoMsg -> IO ()
sendMetrics (Transaction o r a _ _ _ _) m = do
    writeInChan m $ Metric $ increment "cl.tx.count"
    writeInChan m $ Metric $ set "cl.tx.wallet" o
    writeInChan m $ Metric $ set "cl.tx.wallet" r
    writeInChan m $ Metric $ gauge "cl.tx.amount" a


-- generateNTransactions :: ManagerMiningMsg a => QuantityTx -> Chan a -> Chan InfoMsg -> IO (Result ())
generateNTransactions :: QuantityTx -> InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result ())
generateNTransactions qTx ch m = try $ do
  tx <- B.genNTx qTx
  mapM_ (\x -> do
          aMVar <- newEmptyMVar
          writeInChan ch $ NewTransaction x aMVar
          sendMetrics x m
        ) tx
  putStrLn "Transactions are created"

generateTransactionsForever :: InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result ())
generateTransactionsForever ch m = try $ forever $ do
                                quantityOfTranscations <- randomRIO (20,30)
                                tx <- B.genNTx quantityOfTranscations
                                mapM_ (\x -> do
                                            aMVar <- newEmptyMVar
                                            writeInChan ch $ NewTransaction x aMVar
                                            sendMetrics x m
                                       ) tx
                                threadDelay (10^(6 :: Int))
                                putStrLn ("Bundle of " ++ show quantityOfTranscations ++"Transactions was created")


-- safe way to write in chan
writeInChan :: InChan t -> t -> IO ()
writeInChan aChan aMsg = do
    aOk <- tryWriteChan aChan aMsg
    threadDelay 10000
    unless aOk $ writeInChan aChan aMsg
