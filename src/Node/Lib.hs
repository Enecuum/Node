{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Node.Lib where

import              Control.Monad
import              Control.Exception
import              Control.Concurrent
import qualified    Data.ByteString             as B
import qualified    Data.ByteString.Lazy        as L
import              Data.Serialize              as S
import              Data.IORef
import qualified    Data.Aeson as A
import              Lens.Micro
import              Service.Types
import              Network.Socket (tupleToHostAddress)
import Node.FileDB.FileDB
import Node.Node.Types
import Node.Node.Config.Make

import Node.Node.Base.Server

import Service.System.Directory (getTransactionFilePath)
import Service.Network.Base
import System.Environment
import Service.InfoMsg (InfoMsg)
import Node.Data.Key
import Node.FileDB.FileServer
--tmp
import System.Directory (createDirectoryIfMissing)

-- code exemples:
-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
-- docs:
-- https://github.com/ethereum/devp2p/blob/master/rlpx.md
-- https://github.com/ethereum/wiki/wiki/%C3%90%CE%9EVp2p-Wire-Protocol
-- https://www.stackage.org/haddock/lts-10.3/network-2.6.3.2/Network-Socket-ByteString.html

-- | Standart function to launch a node.
startNode :: (NodeConfigClass s, ManagerMsg a1, ToManagerData s) =>
       BuildConfig
    -> Chan ExitMsg
    -> Chan Answer
    -> Chan InfoMsg
    -> (Chan a1 -> IORef s -> IO ())
    -> (Chan a1 -> Chan Transaction -> Chan Microblock -> MyNodeId -> Chan FileActorRequest -> IO a2)
    -> IO (Chan a1)
startNode buildConf exitCh answerCh infoCh manager startDo = do

    --tmp
    createDirectoryIfMissing False "data"

    managerChan <- newChan
    aMicroblockChan <- newChan
    aTransactionChan <- newChan
    config  <- readNodeConfig
    bnList  <- readBootNodeList $ bootNodeList buildConf
    aFileRequesChan <- newChan
    void $ forkIO $ startFileServer aFileRequesChan
    let portNumber = extConnectPort buildConf
    md      <- newIORef $ toManagerData aTransactionChan aMicroblockChan exitCh answerCh infoCh aFileRequesChan bnList config portNumber
    startServerActor managerChan portNumber
    aFilePath <- getTransactionFilePath
    void $ forkIO $ microblockProc aMicroblockChan aFilePath
    void $ forkIO $ manager managerChan md
    void $ startDo managerChan aTransactionChan aMicroblockChan (config^.myNodeId) aFileRequesChan
    return managerChan

microblockProc :: Chan Microblock -> String -> IO b
microblockProc aMicroblockCh aFilePath = forever $ do
        aMicroblock <- readChan aMicroblockCh
        aBlocksFile <- try $ readHashMsgFromFile aFilePath
        aBlocks <- case aBlocksFile of
            Right aBlocks      -> return aBlocks
            Left (_ :: SomeException) -> do
                 B.writeFile aFilePath $ S.encode ([] :: [Microblock])
                 return []
        B.writeFile aFilePath $ S.encode (aBlocks ++ [aMicroblock])


readNodeConfig :: IO NodeConfig
readNodeConfig =
    try (L.readFile "configs/nodeInfo.json") >>= \case
        Right nodeConfigMsg         -> case A.decode nodeConfigMsg of
            Just nodeConfigData     -> return nodeConfigData
            Nothing                 -> putStrLn "Config file can not be readed. New one will be created" >> config
        Left (_ :: SomeException)   -> putStrLn "ConfigFile will be created." >> config
  where
    config = do
        makeFileConfig
        readNodeConfig

readBootNodeList :: String -> IO BootNodeList
readBootNodeList conf = do
    bnList  <- try (getEnv "bootNodeList") >>= \case
            Right item              -> return item
            Left (_::SomeException) -> return conf
    toNormForm $ read bnList
     where
       toNormForm aList = return $ (\(a,b,c) -> (NodeId a, Connect (tupleToHostAddress b) c))
          <$> aList

---



mergeMBlocks :: [Microblock] -> [Microblock] -> [Microblock] -- new old result
mergeMBlocks [] old = old
mergeMBlocks (x:xs) olds = if containMBlock x olds
    then mergeMBlocks xs olds
    else mergeMBlocks xs (x : olds)


containMBlock :: Microblock -> [Microblock] -> Bool
containMBlock el elements = or $ (\(Microblock h1 _ _) (Microblock h2 _ _) -> h1 == h2 ) <$> [el] <*> elements
-------------------------------------------------------
