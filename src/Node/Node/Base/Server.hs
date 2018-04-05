{-# LANGUAGE
        LambdaCase
    ,   ViewPatterns
    ,   MultiWayIf
    ,   ScopedTypeVariables
#-}
module Node.Node.Base.Server where

import qualified    Network.WebSockets                  as WS
import              Service.Network.WebSockets.Server
import              Service.Network.Base
import              Control.Monad.State.Lazy
import              Data.Serialize
import              Control.Concurrent.Async
import              Control.Concurrent.Chan
import              Control.Concurrent
import              Control.Exception
import              Node.Node.Types
import              Node.Crypto
import              Node.Data.NodeTypes
import              Node.Data.NetPackage

startServerActor :: ManagerMsg a => Chan a -> PortNumber -> IO ()
startServerActor aOutputChan aPort = do
    void $ forkIO $ runServer 0 (fromEnum aPort) $
        \aHostAdress pending -> do
            aConnect <- WS.acceptRequest pending
            WS.forkPingThread aConnect 30
            aMsg <- WS.receiveData aConnect
            case decode aMsg of
                Right (conMsg@(Unciphered (ConnectingRequest _ aId _)))
                    | verifyConnectingRequest conMsg -> do
                            aInputChan <- newChan
                            writeChan aOutputChan $
                                initDatagram aInputChan aHostAdress aMsg
                            socketActor
                                aHostAdress
                                (toNodeId aId)
                                aOutputChan
                                aInputChan
                                aConnect
                Right (Unciphered PingRequest) -> do
                    WS.sendBinaryData aConnect $ encode $
                        PongResponce aHostAdress
                _     -> pure ()


socketActor
    ::  ManagerMsg a
    =>  HostAddress
    ->  NodeId
    ->  Chan a
    ->  Chan MsgToSender
    ->  WS.Connection
    ->  IO ()
socketActor _ aId aChan aInputChan aConnect = do
    (void $ race sender receiver) `finally`
        (writeChan aChan $ clientIsDisconnected aId aInputChan)
  where
    sender :: IO ()
    sender = readChan aInputChan >>= \case
        MsgToSender aMsg  -> do
            WS.sendBinaryData aConnect aMsg >> sender
        SenderExit aMsg   -> do
            WS.sendBinaryData aConnect aMsg
        SenderTerminate -> pure ()

    receiver :: IO ()
    receiver = forever $ do
        aMsg <- WS.receiveDataMessage aConnect
        writeChan aChan $ datagramMsg (WS.fromDataMessage aMsg) aId
