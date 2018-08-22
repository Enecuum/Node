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

import           Control.Monad                      (forever)
import           Control.Exception                  (SomeException, try)
import           Control.Monad.Except               (runExceptT)
import           Data.Aeson                         (ToJSON)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8         as BC (putStrLn)
import           Data.DeriveTH
import           Data.List                          (find)
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
import           System.Exit                        (exitWith, ExitCode(..))
import           Service.System.Directory           (getKeyFilePath)

data Flag = Key | ShowKey | Balance PublicKey | Send1 Trans | Send2 Send | Help
          | Quit deriving (Eq, Show)

data ArgFlag = Port PortNumber | Host HostName deriving (Eq, Show)
derive makeIs ''ArgFlag

args :: [OptDescr ArgFlag]
args = [
    Option ['P']      ["port"]    (ReqArg (Port . read) "port") "port number"
  , Option ['A']      ["addr"]    (ReqArg Host "hostAddr")  "host address"
  ]

options :: [OptDescr Flag]
options = [
    Option ['K'] ["get-public-key"] (NoArg Key) "get public key"
  , Option ['B'] ["get-balance"] (ReqArg (Balance . read) "publicKey") "get balance for public key"
  , Option ['M'] ["show-my-keys"] (NoArg ShowKey) "show my public keys"
  , Option ['S'] ["send-money-to-from"] (ReqArg (Send1 . read) "amount:to:from:currency") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
  , Option ['H'] ["help"] (NoArg Help) "show help"
  , Option ['s'] ["send-money-to-from"] (ReqArg (Send2 . read) "Send \"to\" \"from\" currency ") "send money to wallet from wallet (ENQ | ETH | DASH | BTC)"
  , Option ['Q'] ["quit"] (NoArg Quit) "exit"
  ]


control :: IO ()
control = do
    argv   <- getArgs
    case getOpt Permute args argv of
      (flags, _, []) -> do
        addr <- case find isHost flags of
                 Just (Host a) -> return a
                 Nothing       -> return "46.21.248.176"

        port <- case find isPort flags of
                 Just (Port p) -> return p
                 Nothing       -> return 1555
        putStrLn $ usageInfo "Usage: " options
        forever $ do
                  req <- splitOn " " <$> getLine
                  case getOpt Permute options req of
                    (opts, _, []) -> dispatch opts addr port
                    (_, _, err)    -> putStrLn $ concat err ++ usageInfo "Usage: " options

      (_, _, err)    -> ioError (userError (concat err ++ usageInfo "Usage: enq-cli [OPTION...]" args))

dispatch :: [Flag] -> HostName -> PortNumber -> IO ()
dispatch flags h p =
      case flags of
        (Key : _)                    -> getKey
        (Balance aPublicKey : _)     -> withClient $ getBalance aPublicKey
        (Send1 tx : _)               -> withClient $ sendTrans tx
        (Send2 s : _)                -> withClient $ sendTrans $ fromSend s
        (ShowKey : _)                -> showPublicKey
        (Help : _)                   -> putStrLn $ usageInfo "Usage: " options
        (Quit : _)                   -> exitWith ExitSuccess
        _                            -> putStrLn "Wrong argument"

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
sendTrans (Trans am from to _) ch = do
  keyPairs  <- getSavedKeyPairs
  let mapPubPriv = fromList keyPairs :: (Map PublicKey PrivateKey)

  case (Data.Map.lookup from mapPubPriv) of
      Nothing -> putStrLn "You don't own public key"
      Just ownerPrivKey -> do
        uuid <- randomRIO (1,25)
        let tx  = Transaction from to am ENQ Nothing Nothing uuid

        sign  <- getSignature ownerPrivKey tx
        let signTx  = tx { _signature = Just sign }

        result <- runExceptT $ newTx ch signTx
        case result of
          (Left err) -> putStrLn $ "Send transaction error: " ++ show err
          (Right (Hash h) ) -> putStrLn ("Transaction done: ") >> prettyPrint (TransactionAPI signTx h)

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
  result  <- runExceptT $ reqLedger ch rawKey (-1)
  case result of
    (Left err) -> putStrLn $ "Get Balance error: " ++ show err
    (Right (BalanceResp b) ) -> putStrLn $ "Balance: " ++ show b



prettyPrint :: (ToJSON a) => a -> IO ()
prettyPrint r = BC.putStrLn $ encodePretty r
