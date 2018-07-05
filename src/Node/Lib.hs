
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.Lib where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import           Control.Monad
import qualified    Network.WebSockets                  as WS
import           Data.Aeson
import qualified Data.Text                              as T
import qualified Data.ByteString.Lazy                  as L
import              Service.Network.WebSockets.Client
import           Data.IORef
import           Lens.Micro
import           Network.Socket                        (tupleToHostAddress)
import           Node.Data.Key
import           Node.FileDB.FileServer
import           Node.Node.Config.Make
import           Node.Node.Types
import           Service.InfoMsg                       (InfoMsg)
import           Service.Network.Base
import           Service.Transaction.Common            (DBPoolDescriptor (..),
                                                        addMacroblockToDB,
                                                        addMicroblockToDB)
import           Service.Types                         (Microblock, Transaction)
import           System.Directory                      (createDirectoryIfMissing)
import           System.Environment

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
    -> ((InChan MsgToCentralActor, OutChan MsgToCentralActor) -> OutChan Transaction -> OutChan Microblock -> MyNodeId -> InChan FileActorRequest -> IO ())
    -> IO (InChan MsgToCentralActor, OutChan MsgToCentralActor)
startNode descrDB buildConf infoCh manager startDo = do

    --tmp
    createDirectoryIfMissing False "data"

    managerChan <- newChan (128)
    (aMicroblockChan, outMicroblockChan) <- newChan (128)
    (aValueChan, aOutValueChan) <- newChan (128)
    (aTransactionChan, outTransactionChan) <- newChan (128)
    config  <- readNodeConfig
    bnList@[Connect aBootIp aBootPort]  <- readBootNodeList $ bootNodeList buildConf
    runClient (showHostAddress aBootIp) (fromEnum aBootPort) "/" $ \aConnect -> do
        WS.sendTextData aConnect ("{\"tag\":\"Action\",\"type\":\"AddToListOfConnects\",\"port\": 1554}" :: T.Text)
    (aInFileRequestChan, aOutFileRequestChan) <- newChan (16)
    void $ C.forkIO $ startFileServer aOutFileRequestChan
    --let portNumber = extConnectPort buildConf
    md      <- newIORef $ makeNetworkData bnList config infoCh aInFileRequestChan aMicroblockChan aTransactionChan aValueChan
    void $ C.forkIO $ microblockProc descrDB outMicroblockChan aOutValueChan infoCh
    void $ C.forkIO $ manager managerChan md
    void $ startDo managerChan outTransactionChan outMicroblockChan (config^.myNodeId) aInFileRequestChan
    return managerChan


microblockProc :: DBPoolDescriptor -> OutChan Microblock -> OutChan Value -> InChan InfoMsg -> IO ()
microblockProc descriptor aMicroblockCh aValueChan aInfoCh = do
    void $ C.forkIO $ forever $ do
        aMicroblock <- readChan aMicroblockCh
        addMicroblockToDB descriptor aMicroblock aInfoCh
    forever $ do
        aValue <- readChan aValueChan
        addMacroblockToDB descriptor aValue aInfoCh


readNodeConfig :: IO NodeConfig
readNodeConfig =
    try (L.readFile "configs/nodeInfo.json") >>= \case
        Right nodeConfigMsg         -> case decode nodeConfigMsg of
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
