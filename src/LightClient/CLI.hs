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
import Control.Monad (forever)
import Control.Monad.Except (runExceptT, liftIO)
import Control.Exception (SomeException, try)
import Network.Socket (HostName, PortNumber)
import qualified Network.WebSockets as WS

import Service.Types
import Service.Types.PublicPrivateKeyPair
import Service.System.Directory (getTime, getKeyFilePath)
import Service.Network.WebSockets.Client
import LightClient.RPC

data Flag = Key | ShowKey | Balance PublicKey | Send Trans 
          | Block Hash | MBlock Hash | Tx Hash | Wallet PublicKey 
          | GenerateNTransactions QuantityTx | GenerateTransactionsForever 
          | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages 
          | Info | Quit deriving (Eq, Show)

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
  , Option ['B'] ["get-balance"] (ReqArg (Balance . read) "publicKey") "get balance for public key"
  , Option ['M'] ["show-my-keys"] (NoArg ShowKey) "show my public keys"
  , Option ['S'] ["send-money-to-from"] (ReqArg (Send . read) "amount:to:from:currency") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
  , Option ['U'] ["load-block"] (ReqArg (Block . read) "hash") "get keyblock by hash"
  , Option ['O'] ["load-microblock"] (ReqArg (MBlock . read) "hash") "get microblock by hash"
  , Option ['X'] ["get-tx"] (ReqArg (Tx . read) "hash") "get transaction by hash"
  , Option ['W'] ["load-wallet"] (ReqArg (Wallet . read) "publicKey") "gat all transactions in wallet"
-- test
  , Option ['G'] ["generate-n-transactions"] (ReqArg (GenerateNTransactions . read) "qTx") "Generate N Transactions"
  , Option ['F'] ["generate-transactions"] (NoArg GenerateTransactionsForever) "Generate Transactions forever"
  , Option ['A'] ["send-message-for-all"] (ReqArg (SendMessageBroadcast . read) "message") "Send broadcast message"
  , Option ['T'] ["send-message-to"] (ReqArg (SendMessageTo . read) "nodeId message") "Send message to the node"
  , Option ['L'] ["load-new-messages"] (NoArg LoadMessages) "Load new recieved messages"
  , Option ['I'] ["chain-info"] (NoArg Info) "Get total chain info"
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

            putStrLn $ usageInfo "Usage: " options
            forever $ do
                  argv <- splitOn " " <$> getLine
                  case getOpt Permute options argv of
                    (flags, _, []) -> dispatch flags h p
                    (_, _, err)    -> putStrLn $ concat err ++ usageInfo "Usage: " options

getRecipient :: HostName -> PortNumber -> [ArgFlag] -> IO (HostName, PortNumber)
getRecipient defHost defPort []     = return (defHost, defPort)
getRecipient defHost defPort (x:xs) = case x of
         Version  -> do
                     liftIO $ printVersion
                     getRecipient defHost defPort xs
         Port p   -> getRecipient defHost p xs
         Host h   -> getRecipient h defPort xs

dispatch :: [Flag] -> HostName -> PortNumber -> IO ()
dispatch flags h p = 
      case flags of
        (Key : _)                        -> getKey
        (Balance aPublicKey : _)         -> withClient $ getBalance aPublicKey
        (Send tx : _)                    -> withClient $ sendTrans tx
        (ShowKey : _)                    -> showPublicKey
        (Wallet key : _)                 -> withClient $ getAllTransactions key
        (Block hash : _)                 -> withClient $ getBlockByHash hash
        (MBlock hash : _)                -> withClient $ getMicroblockByHash hash
        (Tx hash : _)                    -> withClient $ getTransaction hash

-- test
        (GenerateNTransactions qTx: _)   -> withClient $ generateNTransactions qTx
        (GenerateTransactionsForever: _) -> withClient   generateTransactionsForever
        (SendMessageBroadcast m : _)     -> withClient $ sendMessageBroadcast m
        (SendMessageTo mTo : _)          -> withClient $ sendMessageTo mTo
        (LoadMessages : _)               -> withClient   loadMessages 
        (Info : _)                       -> withClient   getInfo
        (Quit : _)                       -> exitWith ExitSuccess
        _                                -> putStrLn "Wrong argument"

  where withClient f = runClient h (fromEnum p) "" $ \ ch -> f ch

showPublicKey :: IO ()
showPublicKey = do
  pairs <- getSavedKeyPairs
  mapM_ (putStrLn . show . fst) pairs

getKey :: IO ()
getKey = do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  getKeyFilePath >>= (\keyFileName -> appendFile keyFileName (show aPublicKey ++ ":" ++ show aPrivateKey ++ "\n"))
  putStrLn ("Public Key " ++ show aPublicKey ++ " was created")

sendTrans :: Trans -> WS.Connection -> IO ()
sendTrans trans ch = do
  let moneyAmount = txAmount trans
  let receiverPubKey = recipientPubKey trans
  let ownerPubKey = senderPubKey trans
  timePoint <- getTime
  keyPairs <- getSavedKeyPairs
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  case (Data.Map.lookup ownerPubKey mapPubPriv) of
    Nothing -> putStrLn "You don't own this public key"
    Just ownerPrivKey -> do
      sign  <- getSignature ownerPrivKey moneyAmount
      let tx  = Transaction ownerPubKey receiverPubKey moneyAmount ENQ timePoint sign
      print tx
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

getBalance :: PublicKey -> WS.Connection -> IO ()
getBalance rawKey ch = do
  result  <- runExceptT $ reqLedger ch rawKey
  case result of
    (Left err) -> putStrLn $ "Get Balance error: " ++ show err
    (Right b ) -> putStrLn $ "Balance: " ++ show b


generateNTransactions :: Int -> WS.Connection -> IO ()
generateNTransactions qTx ch = do
  result <- runExceptT $ genNTx ch qTx
  case result of
    (Left err) -> putStrLn $ "generateNTransactions error: " ++ show err
    (Right _ ) -> putStrLn   "Transactions request was sent"


generateTransactionsForever :: WS.Connection -> IO ()
generateTransactionsForever ch = do
  result <- runExceptT $ genUnlimTx ch
  case result of
    (Left err) -> putStrLn $ "generateTransactionsForever error: " ++ show err
    (Right _ ) -> putStrLn   "Transactions request was sent"

sendMessageBroadcast :: String -> WS.Connection -> IO ()
sendMessageBroadcast m ch = do
  result <- runExceptT $ newMsgBroadcast ch m
  case result of
    (Left err) -> putStrLn $ "sendMessageBroadcast error: " ++ show err
    (Right _ ) -> putStrLn   "Broadcast message was sent"

sendMessageTo :: MsgTo -> WS.Connection -> IO ()
sendMessageTo mTo ch = do
  result <- runExceptT $ newMsgTo ch mTo
  case result of
    (Left err) -> putStrLn $ "sendMessageTo error: " ++ show err
    (Right _ ) -> putStrLn   "Message was sent"

loadMessages :: WS.Connection -> IO ()
loadMessages ch = do
  result <- runExceptT $ loadNewMsg ch
  case result of
    (Left err)    -> putStrLn $ "sendMessageBroadcast error: " ++ show err
    (Right msgs ) -> putStrLn $ "New messages: " ++ (unlines $ map showMsg msgs)
                  where showMsg (MsgTo id m) = "Message from " ++ show id ++ ": " ++ m

getAllTransactions :: PublicKey -> WS.Connection -> IO ()
getAllTransactions key ch = do
  result <- runExceptT $ getAllTxs ch key
  case result of
    (Left err) -> putStrLn $ "getAllTransactions error: " ++ show err
    (Right txs ) -> mapM_ print txs

getTransaction :: Hash -> WS.Connection -> IO ()
getTransaction hash ch = do
  result <- runExceptT $ getTx ch hash
  case result of
    (Left err) -> putStrLn $ "getTransaction error: " ++ show err
    (Right info ) -> print info

getBlockByHash :: Hash -> WS.Connection -> IO ()
getBlockByHash hash ch = do
  result <- runExceptT $ getBlock ch hash
  case result of
    (Left err) -> putStrLn $ "getBlockByHash error: " ++ show err
    (Right block) -> print block

getMicroblockByHash :: Hash -> WS.Connection -> IO ()
getMicroblockByHash hash ch = do
  result <- runExceptT $ getMicroblock ch hash
  case result of
    (Left err) -> putStrLn $ "getMicroblockByHash error: " ++ show err
    (Right block) -> print block

getInfo :: WS.Connection -> IO ()
getInfo ch = do
  result <- runExceptT $ getChainInfo ch
  case result of
    (Left err) -> putStrLn $ "getChainInfo error: " ++ show err
    (Right info) -> print info

