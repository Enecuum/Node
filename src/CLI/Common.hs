{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

module CLI.Common (
  sendMessageTo,
  sendMessageForAll,
  getMessages,

  sendTrans,
  generateNTransactions,
  generateTransactionsForever,
  getNewKey,
  
  getBalance,
  getPublicKeys,

  Trans(..),
  PubKey,
  QuantityTx
  
   
  )where

import Control.Monad (forever, replicateM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Exception (SomeException, try)
import Data.Time.Units
import Data.List.Split (splitOn)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Data.Map (fromList, lookup, Map)
import System.Random (randomRIO)

import CLI.Balance
import CLI.TransactionsDAG
import Node.Node.Types
import Service.Types
import Service.Types.SerializeJSON ()
import Service.Types.PublicPrivateKeyPair
import Service.InfoMsg
import Service.System.Directory (getTime, getKeyFilePath)

type QuantityTx = Int
type PubKey = String
data Trans = Trans {
        amount :: Amount
      , recipientPubKey :: PubKey
      , senderPubKey :: PubKey
      , currency :: CryptoCurrency
      } deriving (Eq, Show, Generic)

instance ToJSON Trans
instance FromJSON Trans

instance Read Trans where
    readsPrec _ value =
        case splitOn ":" value of
             [f1, f2, f3, f4] ->
                 [(Trans (read f1) f2 f3 (read f4), [])]
             x -> error $ "Invalid number of fields in input: " ++ show x

sendMessageTo = undefined
sendMessageForAll = undefined
getMessages = undefined


sendTrans :: ManagerMiningMsg a => Trans -> Chan a -> Chan InfoMsg -> IO ()
sendTrans trans ch aInfoCh = do
  let moneyAmount = (CLI.Common.amount trans) :: Amount
  let receiverPubKey = read (recipientPubKey trans) :: PublicKey
  let ownerPubKey = read (senderPubKey trans) :: PublicKey
  timePoint <- getTime
  keyPairs <- getSavedKeyPairs
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  case (Data.Map.lookup ownerPubKey mapPubPriv) of
    Nothing -> putStrLn "You don't own that public key"
    Just ownerPrivKey -> do
      sign  <- getSignature ownerPrivKey moneyAmount
      let tx  = WithSignature (WithTime timePoint (SendAmountFromKeyToKey ownerPubKey receiverPubKey moneyAmount)) sign
      sendMetrics tx aInfoCh
      writeChan ch $ newTransaction tx


genNTx :: Int -> IO [Transaction]
genNTx n = do
   let quantityOfKeys = if qKeys <= 2 then 2 else qKeys
                        where qKeys = div n 3
   keys <- replicateM quantityOfKeys generateNewRandomAnonymousKeyPair
   tx <- getTransactions keys n
   return tx

generateNTransactions :: ManagerMiningMsg a =>
    QuantityTx -> Chan a -> Chan InfoMsg -> IO ()
generateNTransactions qTx ch m = do
  tx <- genNTx qTx
  mapM_ (\x -> do
          writeChan ch $ newTransaction x
          sendMetrics x m
        ) tx
  putStrLn "Transactions are created"


generateTransactionsForever :: ManagerMiningMsg a => Chan a -> Chan InfoMsg -> IO b
generateTransactionsForever ch m = forever $ do
                                quantityOfTranscations <- randomRIO (20,30)
                                tx <- genNTx quantityOfTranscations
                                mapM_ (\x -> do
                                            writeChan ch $ newTransaction x
                                            sendMetrics x m
                                       ) tx
                                threadDelay (10^(6 :: Int))
                                putStrLn ("Bundle of " ++ show quantityOfTranscations ++"Transactions was created")

getNewKey :: ManagerMiningMsg a => Chan a -> Chan InfoMsg -> IO PublicKey
getNewKey ch aInfoCh = do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  timePoint <- getTime
  let initialAmount = 0
  let keyInitialTransaction = WithTime timePoint (RegisterPublicKey aPublicKey initialAmount)
  writeChan ch $ newTransaction keyInitialTransaction
  getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
  sendMetrics keyInitialTransaction aInfoCh
  return aPublicKey

getBalance :: PubKey -> Chan InfoMsg -> IO Amount
getBalance key aInfoCh = do
    let pKey = read key :: PublicKey
    stTime  <- ( getCPUTimeWithUnit :: IO Millisecond )
    result  <- countBalance pKey
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


getPublicKeys :: IO [PublicKey]
getPublicKeys = do
  pairs <- getSavedKeyPairs
  return $ map fst pairs


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
