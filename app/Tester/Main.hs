{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Crypto.PubKey.ECC.ECDSA             as ECDSA
import           Data.Aeson
import qualified Data.ByteString                     as B
import qualified Data.Text                           as T
import           Node.Data.Key
import           PoA.PoAServer
import           PoA.Types
import           Service.Network.Base
import           Service.Network.WebSockets.Client
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           System.Environment                  (getArgs)

import qualified Network.WebSockets                  as WS
import           Service.Transaction.TransactionsDAG

data SendMsg where
    SendTransaction     :: Transaction -> SendMsg
    SendMsg             :: NodeId -> SendMsg
    SendHello           :: SendMsg
    SendBroadcas        :: SendMsg
    SendPendingRequest  :: SendMsg
    SendMicroblock      :: Transaction -> SendMsg

data RecivedID          = RecivedID NodeId
data RecivedBroadcast   = RecivedBroadcast NodeId Value
data RecivedMsg         = RecivedMsg       NodeId Value
data RecivedPendingMsg  = RecivedPendingMsg [Transaction]


instance FromJSON RecivedID where
    parseJSON (Object aMsg) = do
        aId <- unhexNodeId =<< aMsg .: "node_id"
        return . RecivedID $ aId

-- TODO idFrom -> sender

instance FromJSON RecivedBroadcast where
    parseJSON (Object aMsg) = do
        aId     <- unhexNodeId =<< aMsg .: "idFrom"
        aMsg    <- aMsg .: "msg"
        return $ RecivedBroadcast aId aMsg


instance FromJSON RecivedMsg where
    parseJSON (Object aMsg) = do
        aId     <- unhexNodeId =<< aMsg .: "sender"
        aMsg    <- aMsg .: "msg"
        return $ RecivedMsg aId aMsg
{-
instance FromJSON RecivedPendingMsg where
    parseJSON (Object aMsg) = do
        aTransactions <-  aMsg .: "transactions"
        return $
-}

testMsg = object [
    "msg" .= ("testMsg" :: String)
  ]

instance ToJSON SendMsg where
    toJSON (SendTransaction aTransaction) = object [
        "tag"           .= ("Request"  :: String),
        "type"          .= ("PendingAdd"  :: String),
        "transaction"   .= aTransaction
      ]

    toJSON SendHello = object [
        "tag"           .= ("Action"  :: String),
        "type"          .= ("Connect"  :: String),
        "node_type"     .= ("PoA" :: String)
      ]

    toJSON SendBroadcas = object [
        "tag"           .= ("Request"  :: String),
        "type"          .= ("Broadcast" :: String),
        "recipientType" .= show All,
        "msg"           .= testMsg
      ]

    toJSON (SendMsg aNodeId) = object [
        "tag"           .= ("Msg"  :: String),
        "type"          .= ("MsgTo" :: String),
        "destination"   .= nodeIdToUnxed aNodeId,
        "msg"           .= testMsg
      ]
    toJSON SendPendingRequest = object [
        "tag"           .= ("Request"  :: String),
        "type"          .= ("Pending" :: String)
      ]


-- @ БН уже поднята
-- @ СН тоже поднята
-- @ мы эмулируем работу двух КН

-- Приконнектиться к БН, взять у неё ip и порт.                     [+]
-- Приконенктиться к СН по ip и порту взятому у БН.                 [+]
-- Создать второй коннект к СН.                                     [+]

-- Просмотреть актуальный список коннектов СН.
-- Между двумя коннектами прокинуть сообщение.                      [+]
    -- Бродкастом.                                                  [+]
    -- Адресное.                                                    [+]

-- Проверить загрузку транзакции в пендинг.
-- Проверить приём микроблока.
-- Проверить, что мы можем просмотреть содержимое пендинга.

connectWithNN aStr aConnect = do
    _ :: B.ByteString <- WS.receiveData aConnect
    putStrLn $ aStr ++ "Sending of hello request"
    WS.sendTextData aConnect $ encode SendHello
    putStrLn $ aStr ++ "Recivin of ID."
    aMsg <- WS.receiveData aConnect
    RecivedID aMyNodeId <- return $ case decode aMsg of
        Just a -> a
        Nothing -> error $
            aStr ++ "FAIL. The recived msg not a response for connect request! " ++ show aMsg
    putStrLn $ aStr ++ "Recived ID = " ++ show aMyNodeId
    return aMyNodeId

main = do
    aArgs <- getArgs
    case aArgs of
        "c":ip:_ -> do
            putStrLn "   --------------------"
            putStrLn "   Start of test script"
            putStrLn "   --------------------"
            aConnectListVar <- newEmptyMVar
            aSecondNodeIsStartedVar <-newEmptyMVar
            testsOk <- newEmptyMVar
            putStrLn "   Connecting to BN..."
            void . forkIO $ runClient ip 1554 "/" $ \aConnect -> do
                putStrLn "   Sending to BN reques for connects..."
                WS.sendTextData aConnect $ encode BNRequestConnects
                putStrLn "   Reciving from BN list of connects..."
                aMsg <- WS.receiveData aConnect
                let BNResponseConnects aConnects = case decode aMsg of
                        Just a -> a
                        Nothing -> error $
                            "   FAIL. The recived msg not a list of connects! " ++ show aMsg
                putMVar aConnectListVar aConnects
            aConnects <- takeMVar aConnectListVar
            putStrLn "   Recived list of NN from BN."

            putStrLn "   Testing firs NN of the list..."
            when (null aConnects) $ error "   FAIL. The recived list is empty."
            let (Connect aHostAddress port):_ = aConnects
            putStrLn "   ---------------------------------"
            putStrLn "   Resending and broadcasting of msg"
            putStrLn "   ---------------------------------"
            aIdOfFirsClientVar <- newEmptyMVar
            putStrLn "1| Connecting CN to NN..."
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                aMyNodeId <- connectWithNN "1| " aConnect
                putStrLn "1| CN is connected to NN."
                putMVar aIdOfFirsClientVar aMyNodeId

                aSecondNodeIsStarted <- takeMVar aSecondNodeIsStartedVar
                putStrLn "1| Sending of test broadcast msg..."
                WS.sendTextData aConnect $ encode SendBroadcas
                aMsg <- WS.receiveData aConnect
                RecivedBroadcast aNodeId aValue <- return $ case decode aMsg of
                    Just a -> a
                    Nothing -> error $
                        "1| FAIL. The recived msg not a response for broadcast! " ++ show aMsg
                putStrLn "1| Broadcast echo recived."
                unless (aNodeId == aMyNodeId) $ error "1| The node ID en broadcast msg is broaken."
                unless (aValue == testMsg)    $ error "1| The broadcast msg is broaken."

                aMsg <- WS.receiveData aConnect
                putStrLn "1| Recived msg from second node."
                RecivedMsg aNodeId aValue <- return $ case decode aMsg of
                    Just a -> a
                    Nothing -> error $
                        "1| FAIL. The recived msg not a correct! " ++ show aMsg
                unless (aValue == testMsg) $ error "1| The broadcast msg is broaken."

                putMVar testsOk True
                return ()

            aIdOfFirsClient <- takeMVar aIdOfFirsClientVar
            putStrLn "2| Connecting of CN to NN..."
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                void $ connectWithNN "2| " aConnect
                putStrLn "2| CN is connected to NN."
                putMVar aSecondNodeIsStartedVar True

                aMsg <- WS.receiveData aConnect
                putStrLn "2| Broadcast msg recived."
                RecivedBroadcast aNodeId aValue <- return $ case decode aMsg of
                    Just a -> a
                    Nothing -> error $
                        "2| FAIL. The recived msg not a broadcast msg! " ++ show aMsg
                unless (aValue == testMsg)    $ error "2| The broadcast msg is broaken."
                putStrLn $ "2| Sending msg to firs node..."
                WS.sendTextData aConnect . encode $ SendMsg aNodeId
                aMsg :: B.ByteString <- WS.receiveData aConnect
                return ()

            _ <- takeMVar testsOk
            putStrLn "   -------"
            putStrLn "   Pending"
            putStrLn "   -------"

            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                aTransaction:_ <- genNTx 10
                void $ connectWithNN "   " aConnect



            putStrLn "   -----------"
            putStrLn "   Testing Ok!"
            putStrLn "   -----------"


        "b":ip:_ -> runClient ip 1554 "/" $ socketActor (\aConnect -> forever $ do
                threadDelay 1000
                WS.sendBinaryData aConnect ("{\"tag\": \"Request\", \"type\": \"Broadcast\", \"recipientType\" : \"PoA\", \"msg\" : { \"str\": \"000000000\"}}" :: T.Text))
        "t":ip:_ -> do
            aTransactions <- genNTx 1000
            putStrLn "Transactions generated"
            runClient ip 1554 "/" $ socketActor $ \aConnect ->
                forM_ (cycle aTransactions) $ \aTrans -> do
                    threadDelay 1000
                    WS.sendBinaryData aConnect $ encode $ SendTransaction aTrans
        _ -> return ()

socketActor aSender aConnect = do
    WS.sendTextData aConnect $ encode SendHello
    void $ race (aSender aConnect) (receiver 0)
  where
    receiver :: Int -> IO ()
    receiver i = do
        aMsg <- WS.receiveDataMessage aConnect
        print i
        print aMsg
        receiver $ i + 1


--
genMicroBlock :: Transaction -> Microblock
genMicroBlock tx = Microblock "123" (ECDSA.Signature 1 2) [] (PublicKey256k1 1) [tx] 1
