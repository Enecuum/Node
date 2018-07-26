{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module LightClient.CLI (
    control,
    Trans(..),
  ) where

import           Control.Monad                      (forM_, replicateM_)
import           Control.Monad.Except               (runExceptT)
import           Data.Aeson                         (ToJSON)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8         as BC (putStrLn)
import           Data.DeriveTH
import           Data.List                          (find, sortBy)
import           Data.List.Split                    (splitOn)
import           Data.Map                           (Map, fromList, lookup)
import           LightClient.RPC
import           Network.Socket                     (HostName, PortNumber)
import qualified Network.WebSockets                 as WS
import           Service.Network.WebSockets.Client
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           System.Console.GetOpt
import           System.Environment                 (getArgs)
import           System.Random


data Flag = Port PortNumber | Host HostName | Version | Help
          | WalletsFile String | TransactionsFile String | KeyGen Int
          | ShowKey String | Balance PublicKey | Info
          | Block Hash | MBlock Hash | Tx Hash | Wallet PublicKey | PartWallet PartWalletReq
          | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages
          | Microblocks | Txs | AllLedger | Kblocks | Chain

     deriving (Eq, Ord, Show)
derive makeIs ''Flag

args :: [OptDescr Flag]
args = [
    Option ['P'] ["port"]    (ReqArg (Port . read) "port") "port number"
  , Option ['A'] ["addr"]    (ReqArg Host "hostAddr")  "host address"

  , Option ['F'] ["wallets"] (ReqArg WalletsFile "walletsFile") "csv file contains wallets"
  , Option ['S'] ["transactions"] (ReqArg TransactionsFile "transactionsFile") "csv file contains transactions should be sent"
  , Option ['G'] ["gen-keys"] (ReqArg (KeyGen . read) "keysCount") "generate N key pairs"

  , Option ['B'] ["get-balance"] (ReqArg (Balance . read) "publicKey") "get balance for public key"
  , Option ['Z'] ["show-my-keys"] (ReqArg ShowKey "wallets") "show my public keys"

  , Option ['K'] ["load-block"] (ReqArg (Block . read) "hash") "get keyblock by hash"
  , Option ['M'] ["load-microblock"] (ReqArg (MBlock . read) "hash") "get microblock by hash"
  , Option ['T'] ["get-tx"] (ReqArg (Tx . read) "hash") "get transaction by hash"
  , Option ['W'] ["load-wallet"] (ReqArg (Wallet . read) "publicKey") "get all transactions for wallet"
  , Option ['Q'] ["load-txs-from-wallet"] (ReqArg (PartWallet . read) "publicKey:offset:count") "get n transactions from k'th tx for given wallet"
  , Option ['I'] ["chain-info"] (NoArg Info) "Get total chain info"
-- test
--  , Option ['R'] ["send-message-for-all"] (ReqArg (SendMessageBroadcast . read) "message") "Send broadcast message"
--  , Option ['T'] ["send-message-to"] (ReqArg (SendMessageTo . read) "nodeId message") "Send message to the node"
--  , Option ['L'] ["load-new-messages"] (NoArg LoadMessages) "Load new recieved messages"

  , Option ['V'] ["version"] (NoArg Version) "show version number"
  , Option ['H', '?'] ["help"] (NoArg Help) "help"


  , Option ['c'] ["get-all-chain"] (NoArg Chain) "load all chain"
  , Option ['l'] ["get-all-ledger"] (NoArg AllLedger) "load all ledger"
  , Option ['m'] ["get-all-microblocks"] (NoArg Microblocks) "load all microblocks"
  , Option ['k'] ["get-all-kblocks"] (NoArg Kblocks) "load all kblocks"
  , Option ['t'] ["get-all-transactios"] (NoArg Txs) "load all transactions"
  ]


control :: IO ()
control = do
    argv   <- getArgs
    case getOpt Permute args argv of
      (flags, _, []) -> do
        addr <- case find isHost flags of
                 Just (Host a) -> return a
                 Nothing       -> return "127.0.0.1"

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
        (Wallet key : _)                 -> withClient $ getAllTransactionsByWallet key
        (PartWallet req : _)             -> withClient $ getPartTransactions req

        (Block hash : _)                 -> withClient $ getBlockByHash hash
        (MBlock hash : _)                -> withClient $ getMicroblockByHash hash
        (Tx hash : _)                    -> withClient $ getTransaction hash
        (Info : _)                       -> withClient   getInfo

-- test
--        (SendMessageBroadcast m : _)     -> withClient $ sendMessageBroadcast m
--        (SendMessageTo mTo : _)          -> withClient $ sendMessageTo mTo
--        (LoadMessages : _)               -> withClient   loadMessages

        (Chain : _)                      -> withClient   getAllChain
        (AllLedger : _)                  -> withClient   getAllLedger
        (Microblocks : _)                -> withClient   getAllMicroblocks
        (Kblocks : _)                    -> withClient   getAllKblocks
        (Txs : _)                        -> withClient   getAllTransactions

        (Help : _)                       -> putStrLn $ usageInfo "Usage: " args
        (Version: _)                     -> printVersion
        _                                -> putStrLn $ usageInfo "Wrong input.\nUsage: " args


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
    case (Data.Map.lookup from mapPubPriv) of
      Nothing -> putStrLn $ "You don't own public key:" ++ show from
      Just ownerPrivKey -> do
        uuid <- randomRIO (1,25)
        let tx  = Transaction from to am ENQ Nothing Nothing uuid

        sign  <- getSignature ownerPrivKey tx
        let signTx  = tx { _signature = Just sign }

        result <- runExceptT $ newTx ch signTx
        case result of
          (Left err) -> putStrLn $ "Send transaction error: " ++ show err
          (Right (Hash h) ) -> putStrLn ("Transaction done: ") >> prettyPrint (TransactionAPI signTx h)

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
                  where showMsg (MsgTo aId m) = "Message from " ++ show aId ++ ": " ++ m


getAllTransactionsByWallet :: PublicKey -> WS.Connection -> IO ()
getAllTransactionsByWallet key ch = do
  result <- runExceptT $ getAllTxs ch key
  case result of
    (Left err)   -> putStrLn $ "getAllTransactions error: " ++ show err
    (Right txs ) -> mapM_ prettyPrint txs


getPartTransactions :: PartWalletReq -> WS.Connection -> IO ()
getPartTransactions req ch = do
  result <- runExceptT $ getPartTxs ch (_key req) (_offset req) (_count req)
  case result of
    (Left err)   -> putStrLn $ "getTransactionsByWallet error: " ++ show err
    (Right txs ) -> mapM_ prettyPrint txs


getTransaction :: Hash -> WS.Connection -> IO ()
getTransaction hash ch = do
  result <- runExceptT $ getTx ch hash
  case result of
    (Left err)    -> putStrLn $ "getTransaction error: " ++ show err
    (Right info ) -> prettyPrint info


getBlockByHash :: Hash -> WS.Connection -> IO ()
getBlockByHash hash ch = do
  result <- runExceptT $ getBlock ch hash
  case result of
    (Left err)    -> putStrLn $ "getBlockByHash error: " ++ show err
    (Right block) -> prettyPrint block


getMicroblockByHash :: Hash -> WS.Connection -> IO ()
getMicroblockByHash aHash ch = do
  result <- runExceptT $ getMicroblock ch aHash
  case result of
    (Left err)    -> putStrLn $ "getMicroblockByHash error: " ++ show err
    (Right block) -> prettyPrint block


getInfo :: WS.Connection -> IO ()
getInfo ch = do
  result <- runExceptT $ getChainInfo ch
  case result of
    (Left err)   -> putStrLn $ "getChainInfo error: " ++ show err
    (Right info) -> prettyPrint info


getAllChain :: WS.Connection -> IO ()
getAllChain ch =  do
  result <- runExceptT $ getAllChainRPC ch
  case result of
    (Left err)    -> putStrLn $ "getAllChain error: " ++ show err
    (Right chain) -> prettyPrint chain

getAllLedger :: WS.Connection -> IO ()
getAllLedger ch =  do
  result <- runExceptT $ getAllLedgerRPC ch
  case result of
    (Left err)     -> putStrLn $ "getAllLedger error: " ++ show err
    (Right ledger) -> prettyPrint ledger

getAllMicroblocks :: WS.Connection -> IO ()
getAllMicroblocks ch =  do
  result <- runExceptT $ getAllMicroblocksRPC ch
  case result of
    (Left err)  -> putStrLn $ "getAllMicroblocks error: " ++ show err
    (Right mbs) -> prettyPrint mbs

getAllKblocks :: WS.Connection -> IO ()
getAllKblocks ch =  do
  result <- runExceptT $ getAllKblocksRPC ch
  case result of
    (Left err)   -> putStrLn $ "getAllKblocks error: " ++ show err
    (Right kbs) -> prettyPrint $ sortBy (\(_, b1) (_, b2) -> compare (_number (b1 :: MacroblockBD)) (_number (b2 :: MacroblockBD))) kbs

getAllTransactions :: WS.Connection -> IO ()
getAllTransactions ch =  do
  result <- runExceptT $ getAllTransactionsRPC ch
  case result of
    (Left err)  -> putStrLn $ "getAllTransactions error: " ++ show err
    (Right txs) -> prettyPrint txs


prettyPrint :: (ToJSON a) => a -> IO ()
prettyPrint r = BC.putStrLn $ encodePretty r
