{-# LANGUAGE ScopedTypeVariables, LambdaCase, PackageImports #-}
module Node.Lib where

import              Control.Monad
import              Control.Exception
import              Control.Concurrent
import qualified    Data.ByteString             as B
import              Data.Serialize              as S
import              Data.IORef

import              Lens.Micro
import              Service.Types


import              Node.Data.NodeTypes
import Node.FileDB.FileDB
import Node.Node.Types
import Node.Node.Config.Make

import Node.Node.Base.Server

import Service.System.Directory (getTransactionFilePath)

-- code exemples:
-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
-- docs:
-- https://github.com/ethereum/devp2p/blob/master/rlpx.md
-- https://github.com/ethereum/wiki/wiki/%C3%90%CE%9EVp2p-Wire-Protocol
-- https://www.stackage.org/haddock/lts-10.3/network-2.6.3.2/Network-Socket-ByteString.html

-- | Standart function to launch a node.
startNode :: (NodeConfigClass s, ManagerMsg a1, ToManagerData s) =>
    String
    -> Chan ExitMsg
    -> Chan Answer
    -> (Chan a1 -> IORef s -> IO ())
    -> (Chan a1 -> Chan Transaction -> MyNodeId -> IO a2)
    -> IO (Chan a1)
startNode path exitCh answerCh manager startDo = do
    managerChan <- newChan
    aMicroblockChan <- newChan
    aTransactionChan <- newChan
    config  <- readNodeConfig path aTransactionChan aMicroblockChan exitCh answerCh
    md      <- newIORef config
    startServerActor managerChan (config^.portNumber)
    aFilePath <- getTransactionFilePath
    void $ forkIO $ forever $ do
        aMicroblock <- readChan aMicroblockChan
        aBlocksFile <- try $ readHashMsgFromFile aFilePath
        aBlocks <- case aBlocksFile of
            Right aBlocks      -> return aBlocks
            Left (_ :: SomeException) -> do
                 B.writeFile aFilePath $ S.encode ([] :: [Microblock])
                 return []
        B.writeFile aFilePath $ S.encode (aBlocks ++ [aMicroblock])
    void $ forkIO $ manager managerChan md
    void $ startDo managerChan aTransactionChan (config^.myNodeId)
    return managerChan


readNodeConfig :: ToManagerData d =>
    String
    -> Chan Transaction
    -> Chan Microblock
    -> Chan ExitMsg
    -> Chan Answer
    -> IO d
readNodeConfig path aTransactionChan aMicroblockChan exitCh answerCh = do
    try (B.readFile path) >>= \case
        Right nodeConfigMsg         -> case decode nodeConfigMsg of
            Right nodeConfigData    -> do
                bnList <- readBootNodeList
                return $ toManagerData
                    aTransactionChan
                    aMicroblockChan
                    exitCh
                    answerCh
                    bnList
                    nodeConfigData
            Left _                  -> config
        Left (_ :: SomeException)   -> config
  where
    config :: ToManagerData d => IO d
    config = do
        bnList <- readBootNodeList
        emptyData defaultServerPort aTransactionChan aMicroblockChan exitCh answerCh bnList

---



mergeMBlocks :: [Microblock] -> [Microblock] -> [Microblock] -- new old result
mergeMBlocks [] old = old
mergeMBlocks (x:xs) olds = if (containMBlock x olds)
                       then mergeMBlocks xs olds
                       else mergeMBlocks xs (x : olds)


containMBlock :: Microblock -> [Microblock] -> Bool
containMBlock el elements = or $ (\(Microblock h1 _ _) (Microblock h2 _ _) -> h1 == h2 ) <$> [el] <*> elements
-------------------------------------------------------
