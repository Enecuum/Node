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


module Enecuum.Legacy.CLI.CLI (
    serveCLI
  ) where

import           Control.Concurrent.Chan.Unagi.Bounded             (InChan)
import           Control.Monad                                     (forever)
import           Data.DeriveTH                                     (derive,
                                                                    makeIs)
import           Data.List.Split                                   (splitOn)
import qualified Enecuum.Legacy.CLI.Common                         as C
import           Enecuum.Legacy.Node.Node.Types                    (MsgToCentralActor)
import           Enecuum.Legacy.Refact.Crypto.PublicPrivateKeyPair (PublicKey)
import           Enecuum.Legacy.Service.System.Version             (version)
import           Enecuum.Legacy.Service.Types                      (Common (..), DBPoolDescriptor,
                                                                    Hash (..),
                                                                    InContainerChan,
                                                                    InfoMsg (..),
                                                                    MsgTo,
                                                                    PartWalletReq (..),
                                                                    Trans (..))
import           Prelude
import           System.Console.GetOpt                             (ArgDescr (..),
                                                                    ArgOrder (..),
                                                                    OptDescr,
                                                                    OptDescr (..),
                                                                    getOpt,
                                                                    usageInfo)


data Flag = Version | Help | Key | Send Trans
          | ShowKey | Balance PublicKey
          | Block Hash | MBlock Hash | Tx Hash | Wallet PublicKey | PartWallet PartWalletReq
          | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages
          | Microblocks | Txs | AllLedger | Kblocks | Chain | Tables

     deriving (Eq, Ord, Show)
derive makeIs ''Flag

options :: [OptDescr Flag]
options =
    [ Option ['K'] ["get-public-key"] (NoArg Key) "get public key"
    , Option ['S']
             ["send-money-to-from"]
             (ReqArg (Send . read) "amount:to:from:currency")
             "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
    , Option ['B'] ["get-balance"]     (ReqArg (Balance . read) "publicKey") "get balance for public key"
    , Option ['Z'] ["show-my-keys"]    (NoArg ShowKey)                       "show my public keys"
    , Option ['G'] ["load-block"]      (ReqArg (Block . read) "hash")        "get keyblock by hash"
    , Option ['M'] ["load-microblock"] (ReqArg (MBlock . read) "hash")       "get microblock by hash"
    , Option ['T'] ["get-tx"]          (ReqArg (Tx . read) "hash")           "get transaction by hash"
    , Option ['W'] ["load-wallet"]     (ReqArg (Wallet . read) "publicKey")  "get all transactions for wallet"
    , Option ['Q']
             ["load-txs-from-wallet"]
             (ReqArg (PartWallet . read) "publicKey:offset:count")
             "get n transactions from k'th tx for given wallet"
    , Option ['I']      ["chain-info"]           (NoArg Help)         "Get total chain info"
    , Option ['R']      ["send-message-for-all"] (ReqArg (SendMessageBroadcast . read) "message") "Send broadcast message"
    , Option ['T']      ["send-message-to"]      (ReqArg (SendMessageTo . read) "nodeId message") "Send message to the node"
    , Option ['L']      ["load-new-messages"]    (NoArg LoadMessages) "Load new recieved messages"
    , Option ['V']      ["version"]              (NoArg Version)      "show version number"
    , Option ['H', '?'] ["help"]                 (NoArg Help)         "help"
    , Option ['c']      ["get-all-chain"]        (NoArg Chain)        "load all chain"
    , Option ['l']      ["get-all-ledger"]       (NoArg AllLedger)    "load all ledger"
    , Option ['m']      ["get-all-microblocks"]  (NoArg Microblocks)  "load all microblocks"
    , Option ['k']      ["get-all-kblocks"]      (NoArg Kblocks)      "load all kblocks"
    , Option ['t']      ["get-all-transactios"]  (NoArg Txs)          "load all transactions"
    , Option ['D']      ["delete-all-tables"]    (NoArg Tables)       "delete-all-tables"
    ]


serveCLI :: DBPoolDescriptor -> InChan MsgToCentralActor -> InChan InfoMsg -> InContainerChan -> IO ()
serveCLI descrDB ch aInfoCh aContChan = do
    putStrLn $ usageInfo "Usage: " options
    forever $ do
        argv <- (splitOn " ") <$> getLine
        case getOpt Permute options argv of
            (flags, _, [] ) -> dispatch flags
            (_    , _, err) -> putStrLn $ concat err ++ usageInfo "Usage: " options
  where
    dispatch :: [Flag] -> IO ()
    dispatch flags = do
        case flags of
            (Key                      : _) -> C.getNewKey >>= handle
            (Send tx                  : _) -> C.sendNewTrans tx ch aInfoCh >>= handle
            (ShowKey                  : _) -> C.getPublicKeys >>= handleList

            (SendMessageBroadcast m   : _) -> C.sendMessageBroadcast m ch >>= handle
            (SendMessageTo        mTo : _) -> C.sendMessageTo mTo ch >>= handle
            (LoadMessages             : _) -> C.loadMessages ch >>= handle
            (Balance aPublicKey       : _) -> C.getBalance descrDB aPublicKey aInfoCh >>= handle

            (Wallet  key              : _) -> C.getAllTransactionsByWallet (Common descrDB aInfoCh) key >>= handle
            (PartWallet (PartWalletReq key offset cnt) : _) ->
                C.getPartTransactions (Common descrDB aInfoCh) aContChan key (fromEnum offset) (fromEnum cnt) >>= handle

            (Block  hash : _) -> C.getKeyBlockByHash (Common descrDB aInfoCh) hash >>= handle
            (MBlock hash : _) -> C.getBlockByHash (Common descrDB aInfoCh) hash >>= handle
            (Tx     hash : _) -> C.getTransactionByHash (Common descrDB aInfoCh) hash >>= handle
            (Help        : _) -> C.getChainInfo (Common descrDB aInfoCh) >>= handle

            (Chain       : _) -> C.getAllChain (Common descrDB aInfoCh) >>= handle
            (AllLedger   : _) -> C.getAllLedger (Common descrDB aInfoCh) >>= handle
            (Microblocks : _) -> C.getAllMicroblocks (Common descrDB aInfoCh) >>= handle
            (Kblocks     : _) -> C.getAllKblocks (Common descrDB aInfoCh) >>= handle
            (Txs         : _) -> C.getAllTransactions (Common descrDB aInfoCh) >>= handle

            _                 -> putStrLn $ usageInfo "Usage: " options



    handle :: Show a => C.Result a -> IO ()
    handle (Left  err) = print err
    handle (Right a  ) = print a

    handleList :: Show a => C.Result [a] -> IO ()
    handleList (Left  err) = print err
    handleList (Right a  ) = mapM_ print a

printVersion :: IO ()
printVersion = putStrLn $ "Version: " ++ $(version)
