{-# Language OverloadedStrings, PackageImports #-}
import                  Node.Data.Data
import                  Node.Crypto
import                  Data.Serialize
import                  Data.Maybe
import                  Data.Word
import                  Network.Socket
import qualified        Data.ByteString.Lazy      as B
import qualified        Data.ByteArray            as BA
import                  Data.IORef
import                  Crypto.PubKey.ECC.DH
import                  Crypto.PubKey.ECC.ECDSA
import                  Crypto.PubKey.ECC.Types
import                  Crypto.Error
import                  Crypto.PubKey.ECC.Generate

import                  Node.Lib
import                  Boot.Boot
import                  Boot.Types
import                  Node.Node.Mining
import                  Node.Node.Types
import                  Service.Timer
import                  Control.Monad
import                  Data.Monoid
import                  Control.Concurrent
import                  Control.Concurrent.Chan
import                  Control.Concurrent.Async

import                  CLI.CLI
import                  Service.HammingDistance
import                  PoA


main :: IO ()
main = testNet3

testSuit2 = do
    -- control channels
    exitChan    <- newChan
    answerChan  <- newChan
    -- start boot nodes
    bootNodesList <- forM [1] $ \i ->
        startNode ("./data/bootNodeConfigs/bootInitData"<> show i <>".bin")
        exitChan answerChan managerBootNode $ \ch _ _ -> do
            metronomeS 10000 (writeChan ch checkBroadcastNodes)
            metronomeS 9000000 (writeChan ch deleteDeadSouls)

    putStrLn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

    broadcastNodeList <-forM [2..5] $ \i -> do
        threadDelay 1000000
        putStrLn $ "Start bro " <> show i
        startNode ("./data/broadcastNodeConfigs/miningInitData"<> show i <>".bin")
            exitChan answerChan managerMining $ \ch _ _ -> do
                metronomeS 400000 (writeChan ch connectivityQuery)
                metronomeS 300000 (writeChan ch deleteOldestMsg)
                metronomeS 10000000 (writeChan ch deleteDeadSouls)
                metronomeS 3000000 $ writeChan ch deleteOldestVacantPositions
    -- start broadcast nodes 
    broadcastNodeList <-forM [1] $ \i -> do
        threadDelay 1000000
        putStrLn $ "Start bro " <> show i
        startNode ("./data/broadcastNodeConfigs/miningInitData"<> show i <>".bin")
            exitChan answerChan managerMining $ \ch aChan aMyNodeId -> do
                metronomeS 400000 (writeChan ch connectivityQuery)
                metronomeS 300000 (writeChan ch deleteOldestMsg)
                metronomeS 10000000 (writeChan ch deleteDeadSouls)
                metronomeS 3000000 $ writeChan ch deleteOldestVacantPositions
                void $ forkIO $ servePoA (show (1800 + i))  aMyNodeId ch aChan (show (2400 + i))
                void $ forkIO $ generateTransactionsForever ch

{-
                void $ forkIO $ txReceiver (show (2100 + i)) ch
                void $ forkIO $ serveRpc (show (2200 + i)) ch
-}
{-
                void $ forkIO $ do
                    aChan <- newChan
                    writeChan ch $ transactionsRequest 1 aChan
                    msg <- readChan aChan
                    print msg
-}

    putStrLn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    simpleNodeList <-forM [] $ \i -> do
        threadDelay 100000
        putStrLn $ "Start sim " <> show (i :: Int)
        startNode ("./data/simpleNodeConfigs/miningInitData" <> show i <> ".bin")
            exitChan answerChan managerMining $ \ch _ _ -> do
                metronomeS 100000 (writeChan ch connectivityQuery)
                metronomeS 300000 (writeChan ch deleteOldestMsg)
                metronomeS 10000000 (writeChan ch deleteDeadSouls)
                metronomeS 3000000 $ writeChan ch deleteOldestVacantPositions
{-
                void $ forkIO $ servePoA (show (1900 + i)) ch (show (2300 + i))
                void $ forkIO $ txReceiver (show (2000 + i)) ch

    -- Information for check connection inside network
    metronome 1000000 $ do
        print "!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!"
        forM_ broadcastNodeList $ \ch -> do
            writeChan ch stateRequest
        forM_ bootNodesList $ \ch -> do
            writeChan ch stateRequest
        forM_ [1..40] $ \_ -> do
            print ""
            msg <- readChan answerChan
            print msg
-}
{-
    metronome 100000 $ do
        startTestGen "127.0.0.1" "2101"
-}
    void $ readChan exitChan

-- Tests need to be rewritten
testSuit1 = do
    (privateNumber1, publicPoint1) <- genKayPair curve
    (publicKey, privateKey)         <- generate curve
    сonnectingMsg                   <- makeConnectingMsg
        (toMyNodeId $ keyToId publicKey) publicPoint1 privateKey publicKey
    unless (verifyConnectingMsg сonnectingMsg) $
        putStrLn "verifyConnectingMsg remoteConnectingMsg"
