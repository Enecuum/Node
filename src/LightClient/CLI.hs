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
import Data.Map (lookup, Map, fromList)
import Control.Monad.Except (runExceptT, liftIO)
import Control.Exception (SomeException, try)
import Network.Socket (HostName, PortNumber)

import Service.Types
import Service.Types.PublicPrivateKeyPair
import Service.System.Directory (getTime, getKeyFilePath)
import Service.Network.TCP.Client
import Service.Network.Base (ClientHandle)
import LightClient.RPC

data Flag = Key | ShowKey | Balance PubKey | Send Trans | GenerateNTransactions QuantityTx | GenerateTransactionsForever | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages | Quit deriving (Eq, Show)

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
  , Option ['B'] ["get-balance"] (ReqArg (Balance) "publicKey") "get balance for public key"
  , Option ['S'] ["send-money-to-from"] (ReqArg (Send . read) "amount:to:from:currency") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
  , Option ['A'] ["send-message-for-all"] (ReqArg (SendMessageBroadcast . read) "message") "Send broadcast message"
  , Option ['T'] ["send-message-to"] (ReqArg (SendMessageTo . read) "nodeId message") "Send message to the node"
  , Option ['L'] ["load-new-messages"] (NoArg LoadMessages) "Load new recieved messages"
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
        (Key : _)                        -> getKey ch
        (GenerateNTransactions qTx: _)   -> generateNTransactions ch qTx
        (GenerateTransactionsForever: _) -> generateTransactionsForever ch
        (Send tx : _)                    -> sendTrans ch tx
        (ShowKey : _)                    -> showPublicKey
        (Balance aPublicKey : _)         -> getBalance ch aPublicKey
        (SendMessageBroadcast m : _)     -> sendMessageBroadcast ch m
        (SendMessageTo mTo : _)          -> sendMessageTo ch mTo
        (LoadMessages : _)               -> loadMessages ch 
        (Quit : _)                       -> closeAndExit ch
        _                                -> putStrLn "Wrong argument"

closeAndExit :: ClientHandle -> IO ()
closeAndExit ch = do
        closeConnect ch
        exitWith ExitSuccess

showPublicKey :: IO ()
showPublicKey = do
  pairs <- getSavedKeyPairs
  mapM_ (putStrLn . show . fst) pairs

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

sendTrans :: ClientHandle -> Trans -> IO ()
sendTrans ch trans = do
  let moneyAmount = (txAmount trans) :: Amount
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
      result <- runExceptT $ newTx ch tx
      case result of
        (Left err) -> putStrLn $ "Send transaction error: " ++ show err
        (Right _ ) -> putStrLn ("Transaction done: " ++ show trans)

printVersion :: IO ()
printVersion = putStrLn ("--" ++ "1.0.0" ++ "--")


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

getBalance :: ClientHandle -> PubKey -> IO ()
getBalance ch rawKey = do
  result  <- runExceptT $ reqLedger ch rawKey
  case result of
    (Left err) -> putStrLn $ "Get Balance error: " ++ show err
    (Right b ) -> putStrLn $ "Balance: " ++ show b


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

sendMessageBroadcast :: ClientHandle -> String -> IO ()
sendMessageBroadcast ch m = do
  result <- runExceptT $ newMsgBroadcast ch m
  case result of
    (Left err) -> putStrLn $ "sendMessageBroadcast error: " ++ show err
    (Right _ ) -> putStrLn   "Broadcast message was sent"

sendMessageTo :: ClientHandle -> MsgTo -> IO ()
sendMessageTo ch mTo = do
  result <- runExceptT $ newMsgTo ch mTo
  case result of
    (Left err) -> putStrLn $ "sendMessageTo error: " ++ show err
    (Right _ ) -> putStrLn   "Message was sent"

loadMessages :: ClientHandle -> IO ()
loadMessages ch = do
  result <- runExceptT $ loadNewMsg ch
  case result of
    (Left err)    -> putStrLn $ "sendMessageBroadcast error: " ++ show err
    (Right msgs ) -> putStrLn $ "New messages: " ++ (unlines $ map showMsg msgs)
                  where showMsg (MsgTo id m) = "Message from " ++ show id ++ ": " ++ m

