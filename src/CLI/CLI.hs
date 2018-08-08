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


module CLI.CLI (
    serveCLI
  ) where

import           CLI.Common
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad                         (forever, mapM_)
import           Data.DeriveTH
import           Data.List.Split                       (splitOn)
import           Node.Node.Types
import           Service.InfoMsg
import           Service.Types
import           Service.Types.PublicPrivateKeyPair    (PublicKey)
import           System.Console.GetOpt


data Flag = Version | Help | Key | Send Trans
          | ShowKey | Balance PublicKey 
          | Block Hash | MBlock Hash | Tx Hash | Wallet PublicKey | PartWallet PartWalletReq
--          | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages
          | Microblocks | Txs | AllLedger | Kblocks | Chain | Tables

     deriving (Eq, Ord, Show)
derive makeIs ''Flag

options :: [OptDescr Flag]
options = [
    Option ['K'] ["get-public-key"] (NoArg Key) "get public key"
  , Option ['S'] ["send-money-to-from"] (ReqArg (Send . read) "amount:to:from:currency") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"

  , Option ['B'] ["get-balance"] (ReqArg (Balance . read) "publicKey") "get balance for public key"
  , Option ['Z'] ["show-my-keys"] (NoArg ShowKey) "show my public keys"

  , Option ['G'] ["load-block"] (ReqArg (Block . read) "hash") "get keyblock by hash"
  , Option ['M'] ["load-microblock"] (ReqArg (MBlock . read) "hash") "get microblock by hash"
  , Option ['T'] ["get-tx"] (ReqArg (Tx . read) "hash") "get transaction by hash"
  , Option ['W'] ["load-wallet"] (ReqArg (Wallet . read) "publicKey") "get all transactions for wallet"
  , Option ['Q'] ["load-txs-from-wallet"] (ReqArg (PartWallet . read) "publicKey:offset:count") "get n transactions from k'th tx for given wallet"
  , Option ['I'] ["chain-info"] (NoArg Help) "Get total chain info"
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
  , Option ['D'] ["delete-all-tables"] (NoArg Tables) "delete-all-tables"

  ]


serveCLI :: DBPoolDescriptor -> InChan MsgToCentralActor -> InChan InfoMsg -> InContainerChan -> IO ()
serveCLI descrDB ch aInfoCh aContChan = do
      putStrLn $ usageInfo "Usage: " options
      forever $ do
               argv <- splitOn " " <$> getLine
               case getOpt Permute options argv of
                 (flags, _, []) -> dispatch flags
                 (_, _, err)    -> putStrLn $ concat err ++ usageInfo "Usage: " options

    where
          dispatch :: [Flag] -> IO ()
          dispatch flags = do
            case flags of
              (Key : _)                         -> getNewKey >>= handle
              (Send tx : _)                     -> sendNewTrans tx ch aInfoCh >>= handle
              (ShowKey : _)                     -> getPublicKeys >>= handleList

--              (SendMessageBroadcast m : _)     -> sendMessageBroadcast m ch >>= handle
--              (SendMessageTo mTo : _)          -> sendMessageTo mTo ch >>= handle
--              (LoadMessages : _)               -> loadMessages ch >>= handle
              (Balance aPublicKey : _)          -> getBalance descrDB aPublicKey aInfoCh >>= handle

              (Wallet key : _)                  -> getAllTransactionsByWallet (Common descrDB aInfoCh) key >>= handle
              (PartWallet (PartWalletReq key offset cnt) : _) -> getPartTransactions (Common descrDB aInfoCh) aContChan key (fromEnum offset) (fromEnum cnt) >>= handle

              (Block hash : _)                  -> getKeyBlockByHash (Common descrDB aInfoCh) hash >>= handle
              (MBlock hash : _)                 -> getBlockByHash (Common descrDB aInfoCh) hash >>= handle
              (Tx hash : _)                     -> getTransactionByHash (Common descrDB aInfoCh) hash >>= handle
              (Help : _)                        -> getChainInfo (Common descrDB aInfoCh) >>= handle

              (Chain : _)                       -> getAllChain (Common descrDB aInfoCh) >>= handle
              (AllLedger : _)                   -> getAllLedger (Common descrDB aInfoCh) >>= handle
              (Microblocks : _)                 -> getAllMicroblocks (Common descrDB aInfoCh) >>= handle
              (Kblocks : _)                     -> getAllKblocks (Common descrDB aInfoCh) >>= handle
              (Txs : _)                         -> getAllTransactions (Common descrDB aInfoCh) >>= handle
 
--             (Tables: _)                       -> deleteAllDB
              (Help : _)                        -> putStrLn $ usageInfo "Usage: " options


          handle :: Show a => Result a -> IO ()
          handle (Left err) = putStrLn $ show err
          handle (Right a)  = print a

          handleList :: Show a => Result [a] -> IO ()
          handleList (Left err) = putStrLn $ show err
          handleList (Right a)  = mapM_ print a

printVersion :: IO ()
printVersion = putStrLn ("--" ++ "2.0.0" ++ "--")

