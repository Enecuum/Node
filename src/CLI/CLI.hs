{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, LambdaCase #-}


module CLI.CLI (
    control,
    Trans(..),
    -- для тестовой сетки
    generateTransactionsForever
  ) where

import System.Console.GetOpt
import Data.List.Split (splitOn)
import Data.Map (fromList, lookup, Map)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad (forever, forM, replicateM)
import Data.Time.Units (Millisecond, subTime, getCPUTimeWithUnit)

import Service.Types.PublicPrivateKeyPair
import Node.Node.Types
import Service.Types
import Service.System.Directory (getTime, getKeyFilePath)
import Service.Metrics
import System.Random
import Data.Graph.Inductive
import CLI.Balance (countBalance)
import Node.Node.Types (ManagerMiningMsgBase, newTransaction)
import CLI.TransactionsDAG


type QuantityTx = Int
type PubKey = String
data Trans = Trans {
        amount :: Amount
      , recipientPubKey :: PubKey
      , senderPubKey :: PubKey
      , currency :: CryptoCurrency
      } deriving (Eq, Show)


instance Read Trans where
    readsPrec _ value =
        case splitOn ":" value of
             [f1, f2, f3, f4] ->
                 [(Trans (read f1) f2 f3 (read f4), [])]
             x -> error $ "Invalid number of fields in input: " ++ show x

data Flag = Version  | Key | ShowKey | Balance PubKey | Send Trans | GenerateNTransactions QuantityTx | GenerateTransactionsForever deriving (Eq, Show)




options :: [OptDescr Flag]
options = [
    Option ['V', '?'] ["version"] (NoArg Version) "show version number"
  , Option ['K'] ["get-public-key"] (NoArg Key) "get public key"
  , Option ['G'] ["generate-n-transactions"] (ReqArg (GenerateNTransactions . read) "qTx") "Generate N Transactions"
  , Option ['F'] ["generate-transactions"] (NoArg GenerateTransactionsForever) "Generate Transactions forever"
  , Option ['M'] ["show-my-keys"] (NoArg ShowKey) "show my public keys"
  , Option ['B'] ["get-balance"] (ReqArg Balance "publicKey") "get balance for public key"
  , Option ['S'] ["send-money-to-from"] (ReqArg (Send . read) "amount:to:from:currency") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
  ]


control :: Chan ManagerMiningMsgBase -> IO ()
control ch = do
    argv <- splitOn " " <$> getLine
    case getOpt Permute options argv of
        (flags, _, []) -> dispatch flags ch
        (_, _, err)  -> ioError (userError (concat err ++ usageInfo header options))
    control ch
      where header = "Usage: eneqm-control [OPTION...] "

dispatch :: [Flag] -> Chan ManagerMiningMsgBase -> IO ()
dispatch flags ch = do
    case flags of
        (Key : _)               -> getKey ch
        (GenerateNTransactions qTx: _) -> generateNTransactions qTx ch
        (GenerateTransactionsForever: _) -> generateTransactionsForever ch
        (Send tx : _)           -> sendTrans tx ch
        (Version : _)           -> printVersion flags
        (ShowKey : _)           -> showPublicKey
        (Balance aPublicKey : _) -> getBalance aPublicKey
        _                       -> printVersion flags

showPublicKey :: IO ()
showPublicKey = do
  pairs <- getSavedPublicKey
  mapM_ (putStrLn . show . fst) pairs

getSavedPublicKey :: IO [(PublicKey, PrivateKey)]
getSavedPublicKey = do
  keyFileContent <- getKeyFilePath >>= (\keyFileName -> readFile keyFileName)
  let rawKeys = lines keyFileContent
  let keys = map (splitOn ":") rawKeys
  let pairs = map (\x -> (,) (read (x !! 0) :: PublicKey) (read (x !! 1) :: PrivateKey)) keys
  return pairs


sendTrans :: Trans -> Chan ManagerMiningMsgBase -> IO ()
sendTrans trans ch = do
  let moneyAmount = (CLI.CLI.amount trans) :: Amount
  let receiverPubKey = read (recipientPubKey trans) :: PublicKey
  let ownerPubKey = read (senderPubKey trans) :: PublicKey
  timePoint <- getTime
  keyPairs <- getSavedPublicKey
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  case (Data.Map.lookup ownerPubKey mapPubPriv) of
    Nothing -> putStrLn "You don't own that public key"
    Just ownerPrivKey -> do
      sign  <- getSignature ownerPrivKey moneyAmount
      let tx  = WithSignature (WithTime timePoint (SendAmountFromKeyToKey ownerPubKey receiverPubKey moneyAmount)) sign
      writeChan ch $ newTransaction tx
      sendMetrics tx
      putStrLn ("Transaction done! " ++ show trans )

printVersion :: (Show a) => [a] -> IO ()
printVersion _ = putStrLn ("--" ++ "1.0.0" ++ "--")

getKey :: Chan ManagerMiningMsgBase -> IO ()
getKey ch = do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  timePoint <- getTime
  let initialAmount = 0
  let keyInitialTransaction = WithTime timePoint (RegisterPublicKey aPublicKey initialAmount)
  writeChan ch $ newTransaction keyInitialTransaction
  sendMetrics keyInitialTransaction
  getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
  putStrLn ("Public Key " ++ show aPublicKey ++ " was created")

-- keyAliases :: Map String PublicKey
-- keyAliases = fromList [("main",(read "B0AahQxTCgHLuixT3RYAmYbZSgNg7WV3Qw5zLhMvM1NAc4" :: PublicKey))
--                    ,("favourite",(read "B0G6kCgWt5jkM5o1HmfaA1FRLwThK86AiCNw79pru5Edpo" :: PublicKey))
--                    ,("in-case",(read "B0hrpSs3GegLVF6RJVa252xuwNXPSPoPoaQNFempm3ZiA" :: PublicKey))]

getBalance :: String -> IO ()
getBalance rawKey = do
  stTime  <- getCPUTimeWithUnit :: IO Millisecond
  result  <- countBalance $ parseKey rawKey
  endTime <- getCPUTimeWithUnit :: IO Millisecond
  metric $ timing "cl.ld.time" (subTime endTime stTime)
  putStrLn (show result)

parseKey :: String -> PublicKey
-- parseKey rawKey = fromJust $ Data.Map.lookup rawKey keyAliases
parseKey rawKey = (read rawKey :: PublicKey)



genTxDAG :: Int -> IO [Transaction]
genTxDAG n = do
   keys <- replicateM n generateNewRandomAnonymousKeyPair
   dag <- getTransactionDAG keys
   forM (labEdges dag)  $ \(_, _, tr) -> return tr

generateNTransactions :: ManagerMiningMsg a =>
    Int -> Chan a -> IO ()
generateNTransactions qTx ch = do
  tx <- genTxDAG qTx
  mapM_ (\x -> do
          writeChan ch $ newTransaction x
          sendMetrics x
        ) tx
  putStrLn ("Transactions are created")


generateTransactionsForever :: ManagerMiningMsg a => Chan a -> IO b
generateTransactionsForever ch = forever $ do
                                quantityOfWallets <- randomRIO (20,30)
                                tx <- genTxDAG quantityOfWallets
                                mapM_ (\x -> do
                                            writeChan ch $ newTransaction x
                                            sendMetrics x
                                       ) tx
                                threadDelay (10^(6 :: Int))
                                putStrLn ("Bundle of " ++ show quantityOfWallets ++"Transactions was created")

sendMetrics :: Transaction -> IO ()
sendMetrics (WithTime _ tx) = sendMetrics tx
sendMetrics (WithSignature tx _) = sendMetrics tx
sendMetrics (RegisterPublicKey k b) = do
                           metric $ increment "cl.tx.count"
                           metric $ set "cl.tx.wallet" k
                           metric $ gauge "cl.tx.amount" b
sendMetrics (SendAmountFromKeyToKey o r a) = do
                           metric $ increment "cl.tx.count"
                           metric $ set "cl.tx.wallet" o
                           metric $ set "cl.tx.wallet" r
                           metric $ gauge "cl.tx.amount" a
