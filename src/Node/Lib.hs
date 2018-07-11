
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.Lib where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import              Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified    Network.WebSockets                  as WS
import           Data.Aeson
import          Service.Chan
import qualified Data.Text                              as T
import qualified Data.ByteString.Lazy                  as L
import              Service.Network.WebSockets.Client
import           Data.Maybe
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
import           PoA.Types

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
    -> ((InChan MsgToCentralActor, OutChan MsgToCentralActor) -> OutChan (Transaction, MVar Bool) -> InChan Microblock -> MyNodeId -> InChan FileActorRequest -> IO ())
    -> IO (InChan MsgToCentralActor, OutChan MsgToCentralActor)
startNode descrDB buildConf infoCh manager startDo = do

    --tmp
    createDirectoryIfMissing False "data"

    managerChan <- newChan 128
    (aMicroblockChan, outMicroblockChan) <- newChan 128
    (aValueChan, aOutValueChan) <- newChan 128
    (aTransactionChan, outTransactionChan) <- newChan 128
    config  <- readNodeConfig
    bnList  <- readBootNodeList $ bootNodeList buildConf
    (aInFileRequestChan, aOutFileRequestChan) <- newChan 128
    void $ C.forkIO $ startFileServer aOutFileRequestChan
    md      <- newIORef $ makeNetworkData config infoCh aInFileRequestChan aMicroblockChan aTransactionChan aValueChan
    void . C.forkIO $ microblockProc descrDB outMicroblockChan aOutValueChan infoCh
    void . C.forkIO $ manager managerChan md
    void $ startDo managerChan outTransactionChan aMicroblockChan (config^.myNodeId) aInFileRequestChan
    void $ C.forkIO $ connectManager (poaPort buildConf) bnList aInFileRequestChan
    return managerChan



connectManager :: PortNumber -> [Connect] -> InChan FileActorRequest -> IO ()
connectManager aPortNumber aBNList aConnectsChan = do
    forM_ aBNList $ \(Connect aBNIp aBNPort) -> do
        void . C.forkIO $ runClient (showHostAddress aBNIp) (fromEnum aBNPort) "/" $ \aConnect -> do
            WS.sendTextData aConnect . encode $ AddToListOfConnectsRequest aPortNumber
    aLoop aBNList
  where
    aLoop = \case
        (Connect aBNIp aBNPort):aTailOfList -> do
            aVar <- newEmptyMVar
            writeInChan aConnectsChan $ NumberConnects aVar
            aPotencialConnectNumber <- takeMVar aVar
            when (aPotencialConnectNumber == 0) $ do
                void . C.forkIO $ runClient (showHostAddress aBNIp) (fromEnum aBNPort) "/" $ \aConnect -> do
                    WS.sendTextData aConnect $ encode BNRequestConnects
                    aMsg <- WS.receiveData aConnect
                    let BNResponseConnects aConnects = fromJust $ decode aMsg
                    writeInChan aConnectsChan $ AddToFile aConnects
                C.threadDelay 1000000
                aLoop (aTailOfList ++ [Connect aBNIp aBNPort])
        _       -> return ()


data AddToListOfConnectsRequest = AddToListOfConnectsRequest PortNumber

instance ToJSON AddToListOfConnectsRequest where
    toJSON (AddToListOfConnectsRequest aPort) = object [
            "tag"  .= ("Action" :: T.Text)
        ,   "type" .= ("AddToListOfConnects" :: T.Text)
        ,   "port" .= fromEnum aPort
      ]

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
