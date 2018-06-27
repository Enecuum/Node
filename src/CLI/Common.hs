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
import           Control.Monad                         (forever)
import           Data.List.Split                       (splitOn)
import           Data.Map                              (Map, fromList, lookup)
import           Data.Time.Units
import           System.Random                         (randomRIO)

import           Node.Node.Types
import           Service.InfoMsg
import           Service.System.Directory              (getKeyFilePath, getTime)
import           Service.Transaction.Common            as B (getAllTransactionsDB,
                                                             getBalanceForKey,
                                                             getBlockByHashDB,
                                                             getTransactionByHashDB)
import           Service.Transaction.Storage           (DBPoolDescriptor (..))
import           Service.Transaction.TransactionsDAG   (genNTx)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()

type Result a = Either CLIException a

data CLIException = WrongKeyOwnerException
                  | NotImplementedException -- test
                  | NoSuchPublicKeyInDB
                  | NoSuchMicroBlockDB
                  | NoSuchTransactionDB
                  | OtherException
  deriving Show

instance Exception CLIException


sendMessageTo :: ManagerMiningMsg a => MsgTo -> InChan a -> IO (Result ())
sendMessageTo _ = return $ return $ Left NotImplementedException


sendMessageBroadcast :: ManagerMiningMsg a => String -> InChan a -> IO (Result ())
sendMessageBroadcast _ = return $ return $ Left NotImplementedException


loadMessages :: ManagerMiningMsg a => InChan a -> IO (Result [MsgTo])
loadMessages _ = return $ Left NotImplementedException


getBlockByHash :: ManagerMiningMsg a => DBPoolDescriptor -> Hash -> InChan a -> IO (Result MicroblockAPI)
getBlockByHash db hash _ = try $ do
  mb <- B.getBlockByHashDB db hash
  case mb of
    Nothing -> throw NoSuchMicroBlockDB
    Just m  -> return m


getKeyBlockByHash :: ManagerMiningMsg a => DBPoolDescriptor -> Hash -> InChan a -> IO (Result Macroblock)
getKeyBlockByHash _ _ _ = return $ Left NotImplementedException
 --return =<< Right <$> B.getBlockByHashDB db hash


getChainInfo :: ManagerMiningMsg a => InChan a -> IO (Result ChainInfo)
getChainInfo _ = return $ Left NotImplementedException


getTransactionByHash :: ManagerMiningMsg a => DBPoolDescriptor -> Hash -> InChan a -> IO (Result TransactionInfo)
getTransactionByHash db hash _ = try $ do
  tx <- B.getTransactionByHashDB db hash
  case tx of
    Nothing -> throw  NoSuchTransactionDB
    Just t  -> return t


getAllTransactions :: ManagerMiningMsg a => DBPoolDescriptor -> PublicKey -> InChan a -> IO (Result [Transaction])
getAllTransactions pool key _ = try $ do
  tx <- B.getAllTransactionsDB pool key
  case tx of
    [] -> throw OtherException
    t  -> return t

sendTrans :: ManagerMiningMsg a => Transaction -> InChan a -> InChan InfoMsg -> IO (Result ())
sendTrans tx ch aInfoCh = try $ do
  sendMetrics tx aInfoCh
  writeChan ch $ newTransaction tx


sendNewTrans :: ManagerMiningMsg a => Trans -> InChan a -> InChan InfoMsg -> IO (Result Transaction)
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
      sign <- getSignature ownerPrivKey moneyAmount
      uuid <- randomRIO (1,25)
      let tx  = Transaction ownerPubKey receiverPubKey moneyAmount ENQ timePoint sign uuid
      _ <- sendTrans tx ch aInfoCh
      return tx



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
    writeChan aInfoCh $ Metric $ timing "cl.ld.time" (subTime stTime endTime)
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
    writeChan m $ Metric $ increment "cl.tx.count"
    writeChan m $ Metric $ set "cl.tx.wallet" o
    writeChan m $ Metric $ set "cl.tx.wallet" r
    writeChan m $ Metric $ gauge "cl.tx.amount" a


-- generateNTransactions :: ManagerMiningMsg a => QuantityTx -> Chan a -> Chan InfoMsg -> IO (Result ())
generateNTransactions :: ManagerMiningMsg a => QuantityTx -> InChan a -> InChan InfoMsg -> IO (Result ())
generateNTransactions qTx ch m = try $ do
  tx <- genNTx qTx
  mapM_ (\x -> do
          writeChan ch $ newTransaction x
          sendMetrics x m
        ) tx
  putStrLn "Transactions are created"

generateTransactionsForever :: ManagerMiningMsg a => InChan a -> InChan InfoMsg -> IO (Result ())
-- generateTransactionsForever :: ManagerMiningMsg a => Chan a -> Chan InfoMsg -> IO (Result ())
generateTransactionsForever ch m = try $ forever $ do
                                quantityOfTranscations <- randomRIO (20,30)
                                tx <- genNTx quantityOfTranscations
                                mapM_ (\x -> do
                                            writeChan ch $ newTransaction x
                                            sendMetrics x m
                                       ) tx
                                threadDelay (10^(6 :: Int))
                                putStrLn ("Bundle of " ++ show quantityOfTranscations ++"Transactions was created")
