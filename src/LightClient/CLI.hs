{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, LambdaCase #-}


module LightClient.CLI (
    control,
    Trans(..),
  ) where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import Data.List.Split (splitOn)
import Data.Map (fromList, lookup, Map)
import Control.Monad.Except (runExceptT, liftIO)
import Network.Socket (HostName, PortNumber)
import Control.Exception

import Service.Types.PublicPrivateKeyPair
import Service.Types
import Service.System.Directory (getTime, getKeyFilePath)
import Service.Network.UDP.Client
import LightClient.RPC

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

data Flag = Key | ShowKey | Balance PubKey | Send Trans | GenerateNTransactions QuantityTx | GenerateTransactionsForever | Quit deriving (Eq, Show)

data ArgFlag = Port PortNumber | Host HostName | Version deriving (Eq, Show)

args :: [OptDescr ArgFlag]
args = [
    Option ['P']      ["port"]    (ReqArg (Port . read) "port") "port number"
  , Option ['A']      ["addr"]    (ReqArg Host "hostAddr")  "host address"
  , Option ['V', '?'] ["version"] (NoArg Version)      "show version number"
  ]

options :: [OptDescr Flag]
options = [
    Option ['K'] ["get-public-key"] (NoArg Key) "get public key"
  , Option ['G'] ["generate-n-transactions"] (ReqArg (GenerateNTransactions . read) "qTx") "Generate N Transactions"
  , Option ['F'] ["generate-transactions"] (NoArg GenerateTransactionsForever) "Generate Transactions forever"
  , Option ['M'] ["show-my-keys"] (NoArg ShowKey) "show my public keys"
  , Option ['B'] ["get-balance"] (ReqArg Balance "publicKey") "get balance for public key"
  , Option ['S'] ["send-money-to-from"] (ReqArg (Send . read) "amount:to:from:currency") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
  , Option ['Q'] ["quit"] (NoArg Quit) "exit"
  ]


control :: IO ()
control = do
    argv   <- getArgs
    case getOpt Permute args argv of
      (a, _, [])  -> parseArgs a
      (_, _, err) -> ioError (userError (concat err ++ usageInfo "Usage: enq-cli [OPTION...]" args))
    where parseArgs a = do
            (h,p)  <- getRecipient "localhost" 1555 a
            aHandle <- openConnect h p
            putStrLn $ usageInfo "Usage: " options
            loop aHandle
              where
                loop aHandle = do
                  argv <- splitOn " " <$> getLine
                  case getOpt Permute options argv of
                    (flags, _, []) -> dispatch flags aHandle
                    (_, _, err)    -> putStrLn $ concat err ++ usageInfo "Usage: " options
                  loop aHandle

getRecipient :: HostName -> PortNumber -> [ArgFlag] -> IO (HostName, PortNumber)
getRecipient defHost defPort []     = return (defHost, defPort)
getRecipient defHost defPort (x:xs) = case x of
         Version  -> do
                     liftIO $ printVersion
                     getRecipient defHost defPort xs
         Port p   -> getRecipient defHost p xs
         Host h   -> getRecipient h defPort xs

dispatch :: [Flag] -> ClientHandle -> IO ()
dispatch flags ch = do
    case flags of
        (Key : _)                -> getKey ch
        (GenerateNTransactions qTx: _) -> generateNTransactions ch qTx
        (GenerateTransactionsForever: _) -> generateTransactionsForever ch
        (Send tx : _)            -> sendTrans ch tx
        (ShowKey : _)            -> showPublicKey
        (Balance aPublicKey : _) -> getBalance ch aPublicKey
        (Quit : _)               -> closeAndExit ch
        _                        -> putStrLn "Wrong argument"

closeAndExit :: ClientHandle -> IO ()
closeAndExit ch = do
        closeConnect ch
        exitWith ExitSuccess

showPublicKey :: IO ()
showPublicKey = do
  pairs <- getSavedPublicKey
  mapM_ (putStrLn . show . fst) pairs

getSavedPublicKey :: IO [(PublicKey, PrivateKey)]
getSavedPublicKey = do
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


sendTrans :: ClientHandle -> Trans -> IO ()
sendTrans ch trans = do
  let moneyAmount = (LightClient.CLI.amount trans) :: Amount
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
      result <- runExceptT $ newTx ch tx
      case result of
        (Left err) -> putStrLn $ "Send transaction error: " ++ show err
        (Right _ ) -> putStrLn ("Transaction done: " ++ show trans)

printVersion :: IO ()
printVersion = putStrLn ("--" ++ "1.0.0" ++ "--")

getKey :: ClientHandle -> IO ()
getKey ch = do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  timePoint <- getTime
  let initialAmount = 0
  let keyInitialTransaction = WithTime timePoint (RegisterPublicKey aPublicKey initialAmount)
  result <- runExceptT $ newTx ch keyInitialTransaction
  case result of
    (Left err) -> putStrLn $ "Key creation error: " ++ show err
    (Right _ ) -> do
           getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
           putStrLn ("Public Key " ++ show aPublicKey ++ " was created")

getBalance :: ClientHandle -> String -> IO ()
getBalance ch rawKey = do
  result  <- runExceptT $ reqLedger ch $ parseKey rawKey
  case result of
    (Left err) -> putStrLn $ "Get Balance error: " ++ show err
    (Right b ) -> putStrLn $ "Balance: " ++ show b

parseKey :: String -> PublicKey
parseKey rawKey = (read rawKey :: PublicKey)



generateNTransactions :: ClientHandle -> Int -> IO ()
generateNTransactions ch qTx = do
  result <- runExceptT $ genNTx ch qTx
  case result of
    (Left err) -> putStrLn $ "generateNTransactions error: " ++ show err
    (Right _ ) -> putStrLn   "Transactions request was sent"


generateTransactionsForever :: ClientHandle -> IO ()
generateTransactionsForever ch = do
  result <- runExceptT $ genUnlimTx ch
  case result of
    (Left err) -> putStrLn $ "generateTransactionsForever error: " ++ show err
    (Right _ ) -> putStrLn   "Transactions request was sent"
