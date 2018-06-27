{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, LambdaCase, TemplateHaskell #-}


module LightClient.CLI (
    control,
    Trans(..),
  ) where

import System.Environment (getArgs)
import System.Console.GetOpt
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Map (lookup, Map, fromList)
import Control.Monad (forM_, replicateM_)
import Control.Monad.Except (runExceptT)
import Network.Socket (HostName, PortNumber)
import qualified Network.WebSockets as WS

import Service.Types
import Service.Types.PublicPrivateKeyPair
import Service.System.Directory (getTime)
import Service.Network.WebSockets.Client
import LightClient.RPC
import System.Random
import Data.DeriveTH

data Flag = Port PortNumber | Host HostName | Version | Help
          | WalletsFile String | TransactionsFile String | KeyGen Int
          | ShowKey String | Balance PublicKey | Info
          | Block Hash | MBlock Hash | Tx Hash | Wallet PublicKey
          | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages
     deriving (Eq, Ord, Show)
derive makeIs ''Flag

args :: [OptDescr Flag]
args = [
    Option ['P'] ["port"]    (ReqArg (Port . read) "port") "port number"
  , Option ['A'] ["addr"]    (ReqArg Host "hostAddr")  "host address"

  , Option ['F'] ["wallets"] (ReqArg WalletsFile "walletsFile") "csv file contains wallets"
  , Option ['S'] ["transactions"] (ReqArg TransactionsFile "transactionsFile") "csv file contains transactions should be sent"
  , Option ['K'] ["gen-keys"] (ReqArg (KeyGen . read) "keysCount") "generate N key pairs"

  , Option ['B'] ["get-balance"] (ReqArg (Balance . read) "publicKey") "get balance for public key"
  , Option ['M'] ["show-my-keys"] (ReqArg ShowKey "wallets") "show my public keys"

  , Option ['U'] ["load-block"] (ReqArg (Block . read) "hash") "get keyblock by hash"
  , Option ['O'] ["load-microblock"] (ReqArg (MBlock . read) "hash") "get microblock by hash"
  , Option ['X'] ["get-tx"] (ReqArg (Tx . read) "hash") "get transaction by hash"
  , Option ['W'] ["load-wallet"] (ReqArg (Wallet . read) "publicKey") "get all transactions for wallet"
  , Option ['I'] ["chain-info"] (NoArg Info) "Get total chain info"
-- test
  , Option ['A'] ["send-message-for-all"] (ReqArg (SendMessageBroadcast . read) "message") "Send broadcast message"
  , Option ['T'] ["send-message-to"] (ReqArg (SendMessageTo . read) "nodeId message") "Send message to the node"
  , Option ['L'] ["load-new-messages"] (NoArg LoadMessages) "Load new recieved messages"

  , Option ['V'] ["version"] (NoArg Version) "show version number"
  , Option ['H', '?'] ["help"] (NoArg Help) "help"
  ]


control :: IO ()
control = do
    argv   <- getArgs
    case getOpt Permute args argv of
      (flags, _, []) -> do 
        addr <- case find isHost flags of
                 Just (Host a) -> return a
                 Nothing        -> return "127.0.0.1"

        port <- case find isPort flags of
                 Just (Port p) -> return p
                 Nothing       -> return 1555
        
        dispatch (filter (\e -> not (isPort e || isHost e)) flags) addr port
      (_, _, err)    -> ioError (userError (concat err ++ usageInfo "Usage: enq-cli [OPTION...]" args))

dispatch :: [Flag] -> HostName -> PortNumber -> IO ()
dispatch flags h p =
      case flags of
        (WalletsFile wf : _)             -> do
                           tf <- case find isTransactionsFile flags of
                                Just (TransactionsFile tf)-> return tf
                                Nothing                   -> error "Please, specify file contains transactions"
                           withClient $ sendTrans tf wf

        (TransactionsFile tf : _)        -> do
                           wf <- case find isWalletsFile flags of
                                Just (WalletsFile wf)     -> return wf
                                Nothing                   -> error "Please, specify file contains wallets"
                           withClient $ sendTrans tf wf

        (KeyGen c : _)                   -> genKeys c
        (Balance aPublicKey : _)         -> withClient $ getBalance aPublicKey
        (ShowKey f: _)                   -> showPublicKey f
        (Wallet key : _)                 -> withClient $ getAllTransactions key
        (Block hash : _)                 -> withClient $ getBlockByHash hash
        (MBlock hash : _)                -> withClient $ getMicroblockByHash hash
        (Tx hash : _)                    -> withClient $ getTransaction hash
        (Info : _)                       -> withClient   getInfo

-- test
        (SendMessageBroadcast m : _)     -> withClient $ sendMessageBroadcast m
        (SendMessageTo mTo : _)          -> withClient $ sendMessageTo mTo
        (LoadMessages : _)               -> withClient   loadMessages
        (Help : _)                       -> putStrLn $ usageInfo "Usage: " args
        (Version: _)                     -> printVersion
        _                                -> putStrLn "Wrong argument"

  where withClient f = runClient h (fromEnum p) "" $ \ ch -> f ch

showPublicKey :: String -> IO ()
showPublicKey f = do
  pairs <- getSavedKeyPairs f
  mapM_ (putStrLn . show . fst) pairs

genKeys :: Int -> IO ()
genKeys n = replicateM_ n $ do
  (KeyPair aPublicKey aPrivateKey) <- generateNewRandomAnonymousKeyPair
  putStrLn $ show aPublicKey ++ ";" ++ show aPrivateKey

sendTrans :: String -> String -> WS.Connection -> IO ()
sendTrans transactionsFile walletsFile ch = do
  keyPairs  <- getSavedKeyPairs walletsFile
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)
  
  rawTransactions <- lines <$> readFile transactionsFile
  let transactions = map (\[x,y,z] ->(read x :: PublicKey, read y :: PublicKey, read z :: Amount)) $ 
                     map (splitOn ";") rawTransactions

  forM_ transactions $ \(from, to, am) -> do
    timePoint <- getTime
    case (Data.Map.lookup from mapPubPriv) of
      Nothing -> putStrLn $ "You don't own public key:" ++ show from
      Just ownerPrivKey -> do
        sign  <- getSignature ownerPrivKey am
        uuid <- randomRIO (1,25)
        let tx  = Transaction from to am ENQ timePoint sign uuid

        result <- runExceptT $ newTx ch tx
        case result of
          (Left err) -> putStrLn $ "Send transaction error: " ++ show err
          (Right _ ) -> putStrLn ("Transaction done: " ++ show tx)

printVersion :: IO ()
printVersion = putStrLn ("--" ++ "2.0.0" ++ "--")


getSavedKeyPairs :: String -> IO [(PublicKey, PrivateKey)]
getSavedKeyPairs f = do
  result <- readFile f
  let rawKeys = lines result
  let keys = map (splitOn ";") rawKeys
  let pairs = map (\[x,y] -> (read x :: PublicKey, read y :: PrivateKey)) keys
  return pairs

getBalance :: PublicKey -> WS.Connection -> IO ()
getBalance rawKey ch = do
  result  <- runExceptT $ reqLedger ch rawKey
  case result of
    (Left err) -> putStrLn $ "Get Balance error: " ++ show err
    (Right b ) -> putStrLn $ "Balance: " ++ show b


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
