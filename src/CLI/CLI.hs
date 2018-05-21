{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, LambdaCase #-}


module CLI.CLI (
    serveCLI
  ) where

import System.Console.GetOpt
import Data.List.Split (splitOn)
import Control.Monad (forever, forM_)
import Control.Concurrent
import Node.Node.Types
import Service.InfoMsg
import Service.Types
import CLI.Common
 
data Flag = Key | ShowKey | Balance PubKey | Send Trans | GenerateNTransactions QuantityTx | GenerateTransactionsForever | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages deriving (Eq, Show)


options :: [OptDescr Flag]
options = [
    Option ['K'] ["get-public-key"] (NoArg Key) "get public key"
  , Option ['G'] ["generate-n-transactions"] (ReqArg (GenerateNTransactions . read) "qTx") "Generate N Transactions"
  , Option ['F'] ["generate-transactions"] (NoArg GenerateTransactionsForever) "Generate Transactions forever"
  , Option ['M'] ["show-my-keys"] (NoArg ShowKey) "show my public keys"
  , Option ['B'] ["get-balance"] (ReqArg (Balance . read) "publicKey") "get balance for public key"
  , Option ['S'] ["send-money-to-from"] (ReqArg (Send . read) "amount:to:from:currency") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
  , Option ['A'] ["send-message-for-all"] (ReqArg (SendMessageBroadcast . read) "message") "Send broadcast message"
  , Option ['T'] ["send-message-to"] (ReqArg (SendMessageTo . read) "nodeId message") "Send message to the node"
  , Option ['L'] ["load-new-messages"] (NoArg LoadMessages) "Load new recieved messages"
  ]


serveCLI :: ManagerMiningMsg a => Chan a -> Chan InfoMsg -> IO ()
serveCLI ch aInfoCh = do
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
              (Key : _)                        -> getNewKey ch aInfoCh >>= print
              (GenerateNTransactions qTx: _)   -> generateNTransactions qTx ch aInfoCh
              (GenerateTransactionsForever: _) -> generateTransactionsForever ch aInfoCh
              (Send tx : _)                    -> sendTrans tx ch aInfoCh >>= print
              (ShowKey : _)                    -> do
                                                  keys <- getPublicKeys 
                                                  forM_ keys print
              (SendMessageBroadcast m : _)     -> sendMessageBroadcast m ch
              (SendMessageTo mTo : _)          -> sendMessageTo mTo ch
              (LoadMessages : _)               -> loadMessages ch >>= print
              (Balance aPublicKey : _)         -> getBalance aPublicKey aInfoCh >>= print
              _                        -> putStrLn "Wrong argument"
