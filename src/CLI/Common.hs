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

  getBalance,
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

import Service.Transaction.Balance
import Service.Transaction.TransactionsDAG
import Node.Node.Types
import Service.Types
import Service.Types.SerializeJSON ()
import Service.Types.PublicPrivateKeyPair
import Service.InfoMsg
import Service.System.Directory (getTime, getKeyFilePath)

type Result a = Either CLIException a

data CLIException = WrongKeyOwnerException
                  | NotImplementedException -- test
                  | OtherException
  deriving Show

instance Exception CLIException

sendMessageTo :: ManagerMiningMsg a => MsgTo -> Chan a -> IO (Result ())
sendMessageTo ch = return $ return $ Left NotImplementedException

sendMessageBroadcast :: ManagerMiningMsg a => String -> Chan a -> IO (Result ())
sendMessageBroadcast ch = return $ return $ Left NotImplementedException

loadMessages :: ManagerMiningMsg a => Chan a -> IO (Result [MsgTo])
loadMessages ch = return $ Left NotImplementedException


sendTrans :: ManagerMiningMsg a => Transaction -> Chan a -> Chan InfoMsg -> IO (Result ())
sendTrans tx ch aInfoCh = try $ do
  sendMetrics tx aInfoCh
  writeChan ch $ newTransaction tx

sendNewTrans :: ManagerMiningMsg a => Trans -> Chan a -> Chan InfoMsg -> IO (Result Transaction)
sendNewTrans trans ch aInfoCh = try $ do
  let moneyAmount = (Service.Types.txAmount trans) :: Amount
  let receiverPubKey = read (recipientPubKey trans) :: PublicKey
  let ownerPubKey = read (senderPubKey trans) :: PublicKey
  timePoint <- getTime
  keyPairs <- getSavedKeyPairs
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  case (Data.Map.lookup ownerPubKey mapPubPriv) of
    Nothing -> do
      throw WrongKeyOwnerException
    Just ownerPrivKey -> do
      sign  <- getSignature ownerPrivKey moneyAmount
      let tx  = WithSignature (WithTime timePoint (SendAmountFromKeyToKey ownerPubKey receiverPubKey moneyAmount)) sign
      _ <- sendTrans tx ch aInfoCh
      return tx


genNTx :: Int -> IO [Transaction]
genNTx n = do
   let quantityOfKeys = if qKeys <= 2 then 2 else qKeys
                        where qKeys = div n 3
   keys <- replicateM quantityOfKeys generateNewRandomAnonymousKeyPair
   tx <- getTransactions keys n
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

getNewKey :: ManagerMiningMsg a => Chan a -> Chan InfoMsg -> IO (Result PubKey)
getNewKey ch aInfoCh = try $ do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  timePoint <- getTime
  let initialAmount = 0
  let keyInitialTransaction = WithTime timePoint (RegisterPublicKey aPublicKey initialAmount)
  writeChan ch $ newTransaction keyInitialTransaction
  getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
  sendMetrics keyInitialTransaction aInfoCh
  return $ show aPublicKey

getBalance :: PubKey -> Chan InfoMsg -> IO (Result Amount)
getBalance key aInfoCh = try $ do
    let pKey = read key
    stTime  <- ( getCPUTimeWithUnit :: IO Millisecond )
    result  <- getBalanceForKey pKey
    endTime <- ( getCPUTimeWithUnit :: IO Millisecond )
    writeChan aInfoCh $ Metric $ timing "cl.ld.time" (subTime stTime endTime)
    return result


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


getPublicKeys :: IO (Result [PubKey])
getPublicKeys = try $ do
  pairs <- getSavedKeyPairs
  return $ map (show . fst) pairs


sendMetrics :: Transaction -> Chan InfoMsg -> IO ()
sendMetrics (WithTime _ tx) m = sendMetrics tx m
sendMetrics (WithSignature tx _) m = sendMetrics tx m
sendMetrics (RegisterPublicKey k b) m = do
                           writeChan m $ Metric $ increment "cl.tx.count"
                           writeChan m $ Metric $ set "cl.tx.wallet" k
                           writeChan m $ Metric $ gauge "cl.tx.amount" b
sendMetrics (SendAmountFromKeyToKey o r a) m = do
                           writeChan m $ Metric $ increment "cl.tx.count"
                           writeChan m $ Metric $ set "cl.tx.wallet" o
                           writeChan m $ Metric $ set "cl.tx.wallet" r
                           writeChan m $ Metric $ gauge "cl.tx.amount" a
