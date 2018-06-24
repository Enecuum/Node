{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

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

import Control.Monad (forever, replicateM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Exception
import Data.Time.Units
import Data.List.Split (splitOn)
import Data.Map (fromList, lookup, Map)
import System.Random (randomRIO)

import Service.Transaction.TransactionsDAG
import Node.Node.Types
import Service.Types
import Service.Types.SerializeJSON ()
import Service.Types.PublicPrivateKeyPair
import Service.InfoMsg
import Service.System.Directory (getTime, getKeyFilePath)
import Service.Transaction.Storage (DBPoolDescriptor(..))
import Service.Transaction.Common as B (getBalanceForKey, getMicroBlockByHashDB, getTransactionByHashDB, getKeyBlockByHashDB, getAllTransactionsDB)
import System.Random
import Service.Transaction.TransactionsDAG (genNTx)

type Result a = Either CLIException a

data CLIException = WrongKeyOwnerException
                  | NotImplementedException -- test
                  | NoSuchPublicKeyInDB
                  | NoSuchMicroBlockDB
                  | NoSuchTransactionDB
                  | OtherException
  deriving Show

instance Exception CLIException


sendMessageTo :: ManagerMiningMsg a => MsgTo -> Chan a -> IO (Result ())
sendMessageTo ch = return $ return $ Left NotImplementedException


sendMessageBroadcast :: ManagerMiningMsg a => String -> Chan a -> IO (Result ())
sendMessageBroadcast ch = return $ return $ Left NotImplementedException


loadMessages :: ManagerMiningMsg a => Chan a -> IO (Result [MsgTo])
loadMessages ch = return $ Left NotImplementedException


getBlockByHash :: ManagerMiningMsg a => DBPoolDescriptor -> Hash -> Chan a -> IO (Result MicroblockAPI)
getBlockByHash db hash ch = do
  mb <- B.getMicroBlockByHashDB db hash
  case mb of
    Nothing -> return (Left NoSuchMicroBlockDB)
    Just m -> return (Right m)


getKeyBlockByHash :: ManagerMiningMsg a => DBPoolDescriptor -> Hash -> Chan a -> IO (Result Macroblock)
getKeyBlockByHash db hash ch = return $ Left NotImplementedException
 --return =<< Right <$> B.getBlockByHashDB db hash


getChainInfo :: ManagerMiningMsg a => Chan a -> IO (Result ChainInfo)
getChainInfo ch = return $ Left NotImplementedException


getTransactionByHash :: ManagerMiningMsg a => DBPoolDescriptor -> Hash -> Chan a -> IO (Result TransactionInfo)
getTransactionByHash db hash ch = do
  tx <- B.getTransactionByHashDB db hash
  case tx of
    Nothing -> return (Left NoSuchTransactionDB)
    Just t -> return (Right t)


getAllTransactions :: ManagerMiningMsg a => DBPoolDescriptor -> PublicKey -> Chan a -> IO (Result [Transaction])
getAllTransactions pool key ch = do
  tx <- B.getAllTransactionsDB pool key
  case tx of
    [] -> return (Left OtherException)
    t -> return (Right t)

sendTrans :: ManagerMiningMsg a => Transaction -> Chan a -> Chan InfoMsg -> IO (Result ())
sendTrans tx ch aInfoCh = try $ do
  sendMetrics tx aInfoCh
  writeChan ch $ newTransaction tx


sendNewTrans :: ManagerMiningMsg a => Trans -> Chan a -> Chan InfoMsg -> IO (Result Transaction)
sendNewTrans trans ch aInfoCh = try $ do
  let moneyAmount = (Service.Types.txAmount trans) :: Amount
  let receiverPubKey = recipientPubKey trans
  let ownerPubKey = senderPubKey trans
  timePoint <- getTime
  keyPairs <- getSavedKeyPairs
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  case (Data.Map.lookup ownerPubKey mapPubPriv) of
    Nothing -> do
      throw WrongKeyOwnerException
    Just ownerPrivKey -> do
      sign <- getSignature ownerPrivKey moneyAmount
      uuid <- randomRIO (1,25)
      let tx  = Transaction ownerPubKey receiverPubKey moneyAmount ENQ timePoint sign uuid
      _ <- sendTrans tx ch aInfoCh
      return tx


generateNTransactions :: ManagerMiningMsg a =>
    QuantityTx -> Chan a -> Chan InfoMsg -> IO (Result ())
generateNTransactions qTx ch m = try $ do
  tx <- genNTx qTx
  mapM_ (\x -> do
          writeChan ch $ newTransaction x
          sendMetrics x m
        ) tx
  putStrLn "Transactions are created"


generateTransactionsForever :: ManagerMiningMsg a => Chan a -> Chan InfoMsg -> IO (Result ())
generateTransactionsForever ch m = try $ forever $ do
                                quantityOfTranscations <- randomRIO (20,30)
                                tx <- genNTx quantityOfTranscations
                                mapM_ (\x -> do
                                            writeChan ch $ newTransaction x
                                            sendMetrics x m
                                       ) tx
                                threadDelay (10^(6 :: Int))
                                putStrLn ("Bundle of " ++ show quantityOfTranscations ++"Transactions was created")


getNewKey :: IO (Result PublicKey)
getNewKey = try $ do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
  putStrLn ("Public Key " ++ show aPublicKey ++ " was created")
  return aPublicKey


getBalance :: DBPoolDescriptor -> PublicKey -> Chan InfoMsg -> IO (Result Amount)
getBalance descrDB pKey aInfoCh = do
    stTime  <- ( getCPUTimeWithUnit :: IO Millisecond )
    balance  <- B.getBalanceForKey descrDB pKey
    endTime <- ( getCPUTimeWithUnit :: IO Millisecond )
    writeChan aInfoCh $ Metric $ timing "cl.ld.time" (subTime stTime endTime)
    case balance of
      Nothing -> return (Left NoSuchPublicKeyInDB)
      Just b -> return (Right b)
    --putStrLn "There is no such key in database"
    -- return result


getSavedKeyPairs :: IO [(PublicKey, PrivateKey)]
getSavedKeyPairs = do
  result <- try $ getKeyFilePath >>= (\keyFileName -> readFile keyFileName)
  case result of
    Left ( _ :: SomeException) -> do
          putStrLn "There is no keys"
          return []
    Right keyFileContent       -> do
          let rawKeys = lines keyFileContent
          let keys = map (splitOn ":") rawKeys
          let pairs = map (\x -> (,) (read (x !! 0) :: PublicKey) (read (x !! 1) :: PrivateKey)) keys
          return pairs


getPublicKeys :: IO (Result [PublicKey])
getPublicKeys = try $ do
  pairs <- getSavedKeyPairs
  return $ map fst pairs


sendMetrics :: Transaction -> Chan InfoMsg -> IO ()
sendMetrics (Transaction o r a _ _ _ _) m = do
                           writeChan m $ Metric $ increment "cl.tx.count"
                           writeChan m $ Metric $ set "cl.tx.wallet" o
                           writeChan m $ Metric $ set "cl.tx.wallet" r
                           writeChan m $ Metric $ gauge "cl.tx.amount" a
