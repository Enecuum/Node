{-# LANGUAGE
        LambdaCase
    ,   MultiWayIf
    ,   ScopedTypeVariables
  #-}
module Node.Node.Base.Server where

import              Control.Concurrent.Chan.Unagi.Bounded
import qualified    Network.WebSockets                  as WS
import              Service.Network.WebSockets.Server
import              Service.Network.Base
import              Control.Monad.State.Lazy
import              Data.Serialize
import              Control.Concurrent.Async
import qualified    Control.Concurrent.Chan             as C
import qualified    Control.Concurrent                  as C
import              Control.Exception
import              Node.Node.Types
import              Node.Crypto
import              Node.Data.NetPackage
import              Node.Data.Key

startServerActor :: ManagerMsg a => InChan a -> PortNumber -> IO ()
startServerActor aOutputChan aPort =
    void $ C.forkIO $ runServer aPort $
        \aHostAdress pending -> do
            aConnect <- WS.acceptRequest pending
            WS.forkPingThread aConnect 30
            aMsg <- WS.receiveData aConnect
            case decode aMsg of
                Right (conMsg@(Unciphered (ConnectingRequest _ aId _ _)))
                    | verifyConnectingRequest conMsg -> do
                            aInputChan <- C.newChan
                            writeChan aOutputChan $
                                initDatagram aInputChan aHostAdress aMsg
                            socketActor
                                aHostAdress
                                (toNodeId aId)
                                aOutputChan
                                aInputChan
                                aConnect
                Right (Unciphered PingRequest) ->
                    WS.sendBinaryData aConnect $ encode $
                        PongResponse aHostAdress
                _     -> pure ()


socketActor
    ::  ManagerMsg a
    =>  HostAddress
    ->  NodeId
    ->  InChan a
    ->  C.Chan MsgToSender
    ->  WS.Connection
    ->  IO ()
socketActor _ aId aChan aInputChan aConnect =
    (try (race sender receiver) >>= \case
        Right _ -> return ()
        Left (_ :: SomeException) -> return ()) `finally`
        writeChan aChan (clientIsDisconnected aId aInputChan)
  where
    sender :: IO ()
    sender = C.readChan aInputChan >>= \case
        MsgToSender aMsg  -> WS.sendBinaryData aConnect aMsg >> sender
        SenderExit aMsg   -> WS.sendBinaryData aConnect aMsg
        SenderTerminate -> pure ()

    receiver :: IO ()
    receiver = forever $ do
        aMsg <- WS.receiveDataMessage aConnect
        writeChan aChan $ datagramMsg (WS.fromDataMessage aMsg) aId
