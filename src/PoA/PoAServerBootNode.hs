{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PoA.PoAServerBootNode (
        serverPoABootNode
) where

import              Control.Monad (void, forever, when)
import qualified    Network.WebSockets                  as WS
import              Service.Network.Base
import              Service.Network.WebSockets.Server
import              Service.Network.WebSockets.Client
import              Control.Concurrent.Chan.Unagi.Bounded
import              Service.InfoMsg as I
import qualified    Data.Text as T
import              System.Random.Shuffle
import              Data.Aeson as A
import              Control.Exception
import              Node.Data.GlobalLoging
import              PoA.Types
import qualified    Control.Concurrent as C
import              Node.FileDB.FileServer
import              Data.Maybe()


data ConnectTesterActor = AddConnectToList Connect | TestExistedConnect Connect


serverPoABootNode :: PortNumber -> InChan InfoMsg -> InChan FileActorRequest -> IO ()
serverPoABootNode aRecivePort aInfoChan aFileServerChan = do
    writeLog aInfoChan [ServerBootNodeTag, InitTag] Info $
        "Init. ServerPoABootNode: a port is " ++ show aRecivePort

    (aInChan, aOutChan) <- newChan 64
    void $ C.forkIO $ forever $ readChan aOutChan >>= \case
        AddConnectToList aConn@(Connect aHostAdress aPort) -> void $ C.forkIO $ do
            C.threadDelay 3000000
            runClient (showHostAddress aHostAdress) (fromEnum aPort) "/" $
                \_ -> void $ tryWriteChan aFileServerChan $ AddToFile [aConn]

        TestExistedConnect aConn@(Connect aHostAdress aPort) -> void $ C.forkIO $ do
            aConnects <- getRecords aFileServerChan
            when (aConn`elem`aConnects) $ do
                aOk <- try $ runClient (showHostAddress aHostAdress) (fromEnum aPort) "/" $ \_ -> return ()
                case aOk of
                    Left (_ :: SomeException) ->
                        void $ tryWriteChan aFileServerChan $ DeleteFromFile aConn
                    _ -> return ()

    runServer aRecivePort $ \aHostAdress aPending -> do
        aConnect <- WS.acceptRequest aPending
        writeLog aInfoChan [ServerBootNodeTag] Info "ServerPoABootNode.Connect accepted."
        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right a -> case a of
                RequestPotentialConnects aFull
                    | aFull -> do
                        writeLog aInfoChan [ServerBootNodeTag] Info "Accepted request full list of connections."
                        aConnects <- getRecords aFileServerChan
                        WS.sendTextData aConnect $ A.encode $ ResponsePotentialConnects aConnects

                    | otherwise -> do
                        writeLog aInfoChan [ServerBootNodeTag] Info "Accepted request of connections."
                        aConnects <- getRecords aFileServerChan
                        WS.sendTextData aConnect . A.encode $ ResponsePotentialConnects aConnects

                ActionAddToConnectList aPort ->
                    void $ tryWriteChan aInChan $ AddConnectToList (Connect aHostAdress aPort)

                ActionConnectIsDead aConnect ->
                    void $ tryWriteChan aInChan $ TestExistedConnect aConnect

                _  -> writeLog aInfoChan [ServerBootNodeTag] Warning $
                    "Broken message from PP " ++ show aMsg
            Left a -> do
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"Error\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")
                writeLog aInfoChan [ServerBootNodeTag] Warning $
                    "Broken message from PP " ++ show aMsg ++ " " ++ a ++ showHostAddress aHostAdress

--------------------------------------------------------------------------------
