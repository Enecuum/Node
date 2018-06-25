{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
{-# LANGUAGE PackageImports #-}
module Node.Lib where

import              Control.Monad
import              Control.Exception
import              Control.Concurrent
import qualified    Data.ByteString.Lazy        as L
import              Data.IORef
import qualified    Data.Aeson as A
import              Lens.Micro
import              Service.Types
import              Network.Socket (tupleToHostAddress)
import Node.Node.Types
import Node.Node.Config.Make

import Node.Node.Base.Server
import Service.Network.Base
import System.Environment
import Service.InfoMsg (InfoMsg)
import Node.Data.Key
import Node.FileDB.FileServer
--tmp
import System.Directory (createDirectoryIfMissing)
import Service.Transaction.Common (addMicroblockToDB, DBPoolDescriptor(..))


-- code examples:
-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
-- docs:
-- https://github.com/ethereum/devp2p/blob/master/rlpx.md
-- https://github.com/ethereum/wiki/wiki/%C3%90%CE%9EVp2p-Wire-Protocol
-- https://www.stackage.org/haddock/lts-10.3/network-2.6.3.2/Network-Socket-ByteString.html

-- | Standart function to launch a node.
startNode :: (NodeConfigClass s, ManagerMsg a1, ToManagerData s) =>
       DBPoolDescriptor
    -> BuildConfig
    -> Chan ExitMsg
    -> Chan Answer
    -> Chan InfoMsg
    -> (Chan a1 -> IORef s -> IO ())
    -> (Chan a1 -> Chan Transaction -> Chan Microblock -> MyNodeId -> Chan FileActorRequest -> IO a2)
    -> IO (Chan a1)
startNode descrDB buildConf exitCh answerCh infoCh manager startDo = do

    --tmp
    createDirectoryIfMissing False "data"

    managerChan <- newChan
    aMicroblockChan <- newChan
    aTransactionChan <- newChan
    config  <- readNodeConfig
    bnList  <- readBootNodeList $ bootNodeList buildConf
    aFileRequestChan <- newChan
    void $ forkIO $ startFileServer aFileRequestChan
    let portNumber = extConnectPort buildConf
    md      <- newIORef $ toManagerData aTransactionChan aMicroblockChan exitCh answerCh infoCh aFileRequestChan bnList config portNumber
    startServerActor managerChan portNumber
    void $ forkIO $ microblockProc descrDB aMicroblockChan infoCh
    void $ forkIO $ manager managerChan md
    void $ startDo managerChan aTransactionChan aMicroblockChan (config^.myNodeId) aFileRequestChan
    return managerChan


microblockProc :: DBPoolDescriptor -> Chan Microblock -> Chan InfoMsg -> IO b
microblockProc descriptor aMicroblockCh aInfoCh = forever $ do
        aMicroblock <- readChan aMicroblockCh
        addMicroblockToDB descriptor aMicroblock aInfoCh


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
containMBlock el elements = or $ (==) <$> [el] <*> elements
-------------------------------------------------------
