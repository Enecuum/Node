{-# LANGUAGE
    LambdaCase,
    ViewPatterns,
    MultiWayIf,
    ScopedTypeVariables
  #-}
module Node.Node.Base.Server where

import qualified    Network.WebSockets                  as WS
import              Service.Network.WebSockets.Server
import              Service.Network.WebSockets.Client
import              Service.Network.Base
import              System.Clock
import              System.Random.Shuffle
import              Control.Monad.State.Lazy
import              Control.Monad.Extra
import              Crypto.Error
import              Crypto.PubKey.ECC.ECDSA
import qualified    Data.ByteString                 as B
import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import qualified    Data.Set                        as S
import              Data.IORef
import              Data.Serialize
import              Data.List.Extra
import              Data.Maybe
import              Data.Monoid
import              Lens.Micro.Mtl
import              Lens.Micro
import              Control.Concurrent.Async
import              Control.Concurrent.Chan
import              Control.Concurrent
import              Control.Exception
import              Node.Node.Types
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Node.FileDB.FileDB
import              Node.Extra
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges

startServerActor :: ManagerMsg a => Chan a -> PortNumber -> IO ()
startServerActor aOutputChan aPort = do
    void $ forkIO $ runServer 0 (fromEnum aPort) $
        \aHostAdress pending -> do
            conn <- WS.acceptRequest pending
            WS.forkPingThread conn 30
            aMsg <- WS.receiveData conn
            case decode aMsg of
                Right (conMsg@(Unciphered (ConnectingRequest _ aId _)))
                    | verifyConnectingRequest conMsg -> do
                            aInputChan <- newChan
                            writeChan aOutputChan $
                                initDatagram aInputChan aHostAdress aMsg
                            socketActor aHostAdress (toNodeId aId) aOutputChan aInputChan conn
                _     -> pure ()
--
socketActor ::
    ManagerMsg a
    => HostAddress
    -> NodeId
    -> Chan a
    -> Chan MsgToSender
    -> WS.Connection
    -> IO ()
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
