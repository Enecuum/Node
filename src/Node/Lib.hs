{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}

module Node.Lib where

import              Control.Monad
import              Control.Exception
import qualified    Control.Concurrent          as C
import qualified    Data.ByteString.Lazy        as L
import              Data.IORef
import qualified    Data.Aeson as A
import              Lens.Micro
import              Service.Types
import              Network.Socket (tupleToHostAddress)
import              Control.Concurrent.Chan.Unagi.Bounded
import Node.Node.Types
import Node.Node.Config.Make


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
startNode
    :: DBPoolDescriptor
    -> BuildConfig
    -> InChan InfoMsg
    -> ((InChan MsgToCentralActor, OutChan MsgToCentralActor) -> IORef NetworkNodeData -> IO ())
    -> ((InChan MsgToCentralActor, OutChan MsgToCentralActor) -> InChan Transaction -> InChan Microblock -> MyNodeId -> InChan FileActorRequest -> IO MsgToCentralActor)
    -> IO (InChan MsgToCentralActor, OutChan MsgToCentralActor)
startNode descrDB buildConf infoCh manager startDo = do

    --tmp
    createDirectoryIfMissing False "data"

    managerChan@(inChanManager, _) <- newChan (2^7)
    (aMicroblockChan, outMicroblockChan) <- newChan (2^7)
    (aTransactionChan, outTransactionChan) <- newChan (2^7)
    config  <- readNodeConfig
    bnList  <- readBootNodeList $ bootNodeList buildConf
    (aInFileRequestChan, aOutFileRequestChan) <- newChan (2^4)
    void $ C.forkIO $ startFileServer aOutFileRequestChan
    let portNumber = extConnectPort buildConf
    md      <- newIORef $ makeNetworkData bnList config infoCh aInFileRequestChan aMicroblockChan aTransactionChan
    void $ C.forkIO $ microblockProc descrDB outMicroblockChan infoCh
    void $ C.forkIO $ manager managerChan md
    void $ startDo managerChan aTransactionChan aMicroblockChan (config^.myNodeId) aInFileRequestChan
    return managerChan


microblockProc :: DBPoolDescriptor -> C.Chan Microblock -> InChan InfoMsg -> IO b
microblockProc descriptor aMicroblockCh aInfoCh = forever $ do
        aMicroblock <- C.readChan aMicroblockCh
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

readBootNodeList :: String -> IO [Connect]
readBootNodeList conf = do
    bnList  <- try (getEnv "bootNodeList") >>= \case
            Right item              -> return item
            Left (_::SomeException) -> return conf
    toNormForm $ read bnList
     where
       toNormForm aList = return $ (\(b,c) -> Connect (tupleToHostAddress b) c)
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
