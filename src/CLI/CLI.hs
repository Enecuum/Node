{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, LambdaCase #-}


module CLI.CLI (
    serveCLI
  ) where

import System.Console.GetOpt
import Data.List.Split (splitOn)
import Control.Monad (forever, mapM_)
import qualified    Control.Concurrent as C
import              Control.Concurrent.Chan.Unagi.Bounded
import Node.Node.Types
import Service.InfoMsg
import Service.Types
import Service.Types.PublicPrivateKeyPair (PublicKey)
import CLI.Common
import Service.Transaction.Storage (DBPoolDescriptor(..))

data Flag = Key | ShowKey | Balance PublicKey | Send Trans | GenerateNTransactions QuantityTx | GenerateTransactionsForever | SendMessageBroadcast String | SendMessageTo MsgTo | LoadMessages deriving (Eq, Show)


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


serveCLI :: ManagerMiningMsg a => DBPoolDescriptor -> InChan a -> C.Chan InfoMsg -> IO ()
serveCLI descrDB ch aInfoCh = do
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
              (Key : _)                        -> getNewKey >>= handle
              (GenerateNTransactions qTx: _)   -> generateNTransactions qTx ch aInfoCh >>= handle
              (GenerateTransactionsForever: _) -> generateTransactionsForever ch aInfoCh >>= handle
              (Send tx : _)                    -> sendNewTrans tx ch aInfoCh >>= handle
              (ShowKey : _)                    -> getPublicKeys >>= handleList
              (SendMessageBroadcast m : _)     -> sendMessageBroadcast m ch >>= handle
              (SendMessageTo mTo : _)          -> sendMessageTo mTo ch >>= handle
              (LoadMessages : _)               -> loadMessages ch >>= handle
              (Balance aPublicKey : _)         -> getBalance descrDB aPublicKey aInfoCh >>= handle
              _                                -> putStrLn "Wrong argument"

          handle :: Show a => Result a -> IO ()
          handle (Left err) = putStrLn $ show err
          handle (Right a)  = print a

          handleList :: Show a => Result [a] -> IO ()
          handleList (Left err) = putStrLn $ show err
          handleList (Right a)  = mapM_ print a
