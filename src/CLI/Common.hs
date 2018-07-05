{-# LANGUAGE OverloadedStrings   #-}
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
  getAllTransactions,
  getPartTransactions,
  getBalance,
  getChainInfo,
  getPublicKeys,

  CLIException(..),
  Result

  )where

import           Control.Concurrent                    (threadDelay)
-- import           Control.Concurrent.Chan
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import           Control.Monad                         (forever, void)
import           Data.List.Split                       (splitOn)
import           Data.Map                              (Map, fromList, lookup)
import           Data.Time.Units
import           System.Random                         (randomRIO)

import           Node.Node.Types
import           Service.InfoMsg
import           Service.System.Directory              (getKeyFilePath, getTime)
import           Service.Transaction.Common            as B (getBalanceForKey,
                                                             getBlockByHashDB,
                                                             getKeyBlockByHashDB,
                                                             getLastTransactions,
                                                             getTransactionByHashDB,
                                                             getTransactionsByMicroblockHash)
import           Service.Transaction.Storage           (DBPoolDescriptor,
                                                        getAllTransactionsDB)
import           Service.Transaction.TransactionsDAG   (genNTx)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()

import           Control.Timeout
import           Data.Time.Units                       (Second)

type Result a = Either CLIException a

data CLIException = WrongKeyOwnerException
                  | NotImplementedException -- test
                  | NoTransactionsForPublicKey
                  | NoSuchPublicKeyInDB
                  | NoSuchMicroBlockDB
                  | NoSuchMacroBlockDB
                  | NoSuchTransactionDB
                  | TransactionChanBusyException
                  | OtherException
  deriving Show

instance Exception CLIException


sendMessageTo :: MsgTo -> InChan MsgToCentralActor -> IO (Result ())
sendMessageTo _ _ = return $ return undefined


sendMessageBroadcast :: String -> InChan MsgToCentralActor -> IO (Result ())
sendMessageBroadcast _ = return $ return $ Left NotImplementedException


loadMessages :: InChan MsgToCentralActor -> IO (Result [MsgTo])
loadMessages _ = return $ Left NotImplementedException


getBlockByHash :: DBPoolDescriptor -> Hash -> InChan MsgToCentralActor -> IO (Result MicroblockAPI)
getBlockByHash db hash _ = try $ do
  mb <- B.getBlockByHashDB db hash
  case mb of
    Nothing -> throw NoSuchMicroBlockDB
    Just m  -> return m


getKeyBlockByHash :: DBPoolDescriptor -> Hash -> InChan MsgToCentralActor -> IO (Result MacroblockAPI)
getKeyBlockByHash db hash _ = try $ do
  mb <- B.getKeyBlockByHashDB db hash
  case mb of
    Nothing -> throw NoSuchMacroBlockDB
    Just m  -> return m


getChainInfo :: InChan MsgToCentralActor -> IO (Result ChainInfo)
getChainInfo _ = return $ Left NotImplementedException


getTransactionByHash :: DBPoolDescriptor -> Hash -> InChan MsgToCentralActor -> IO (Result TransactionInfo)
getTransactionByHash db hash _ = try $ do
  tx <- B.getTransactionByHashDB db hash
  case tx of
    Nothing -> throw  NoSuchMicroBlockDB
    Just t  -> return t


getAllTransactions :: DBPoolDescriptor -> PublicKey -> InChan MsgToCentralActor -> IO (Result [TransactionAPI])
getAllTransactions pool key _ = try $ do
  tx <- getAllTransactionsDB pool key
  case tx of
    [] -> throw NoTransactionsForPublicKey
    t  -> return t


getPartTransactions :: DBPoolDescriptor -> PublicKey -> Int -> Int -> InChan MsgToCentralActor -> IO (Result [TransactionAPI])
getPartTransactions pool key offset count _ = try $ do --return $ Left NotImplementedException
  tx <- B.getLastTransactions pool key offset count
  case tx of
    [] -> throw NoTransactionsForPublicKey
    t  -> return t


sendTrans :: Transaction -> InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result ())
sendTrans tx ch aInfoCh = try $ do
  exp <- (timeout (5 :: Second) $ do
           sendMetrics tx aInfoCh
           cTime <- getTime
           void $ tryWriteChan ch $ NewTransaction (tx { _timeMaybe = Just cTime } ))
  case exp of
    Just _  -> return ()
    Nothing -> throw TransactionChanBusyException




sendNewTrans :: Trans -> InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result Transaction)
sendNewTrans aTrans ch aInfoCh = try $ do
  let moneyAmount = Service.Types.txAmount aTrans :: Amount
  let receiverPubKey = recipientPubKey aTrans
  let ownerPubKey = senderPubKey aTrans
  timePoint <- getTime
  keyPairs <- getSavedKeyPairs
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  case Data.Map.lookup ownerPubKey mapPubPriv of
    Nothing -> throw WrongKeyOwnerException
    Just ownerPrivKey -> do
      uuid <- randomRIO (1,25)
      let tx  = Transaction ownerPubKey receiverPubKey moneyAmount ENQ Nothing Nothing uuid
      sign <- getSignature ownerPrivKey tx
      let signTx  = tx { _signature = Just sign }
      _ <- sendTrans signTx ch aInfoCh
      return signTx



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
    void $ tryWriteChan aInfoCh $ Metric $ timing "cl.ld.time" (subTime stTime endTime)
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
    void $ tryWriteChan m $ Metric $ increment "cl.tx.count"
    void $ tryWriteChan m $ Metric $ set "cl.tx.wallet" o
    void $ tryWriteChan m $ Metric $ set "cl.tx.wallet" r
    void $ tryWriteChan m $ Metric $ gauge "cl.tx.amount" a


-- generateNTransactions :: ManagerMiningMsg a => QuantityTx -> Chan a -> Chan InfoMsg -> IO (Result ())
generateNTransactions :: QuantityTx -> InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result ())
generateNTransactions qTx ch m = try $ do
  tx <- genNTx qTx
  mapM_ (\x -> do
          void $ tryWriteChan ch $ NewTransaction x
          sendMetrics x m
        ) tx
  putStrLn "Transactions are created"

generateTransactionsForever :: InChan MsgToCentralActor -> InChan InfoMsg -> IO (Result ())
-- generateTransactionsForever :: ManagerMiningMsg a => Chan a -> Chan InfoMsg -> IO (Result ())
generateTransactionsForever ch m = try $ forever $ do
                                quantityOfTranscations <- randomRIO (20,30)
                                tx <- genNTx quantityOfTranscations
                                mapM_ (\x -> do
                                            void $ tryWriteChan ch $ NewTransaction x
                                            sendMetrics x m
                                       ) tx
                                threadDelay (10^(6 :: Int))
                                putStrLn ("Bundle of " ++ show quantityOfTranscations ++"Transactions was created")
