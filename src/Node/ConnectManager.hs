{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.ConnectManager where

--
import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import           Control.Monad
import           Control.Concurrent.Async
import           Data.Aeson                            as A
import           Data.Maybe
import qualified Network.WebSockets                    as WS
import           Node.Data.GlobalLoging
import           Node.Data.Key
import           Node.DataActor
import           Node.Node.Types
import           Node.NetLvl.Server
import           Node.NetLvl.Massages
import           Service.Chan
import           Service.InfoMsg
import           Service.Network.Base
import           Service.Network.WebSockets.Client
import           Service.Sync.SyncJson
import           Node.SyncServer

--

connectManager aSyncChan (inDBActorChan, _) aManagerChan aPortNumber aBNList aConnectsChan aMyNodeId inChanPending aInfoChan aInLogerChan = do
    writeLog aInfoChan [ConnectingTag, InitTag] Info "Manager of connecting started."
    void $ C.forkIO $ syncServer aSyncChan inDBActorChan aManagerChan aInfoChan
    forM_ aBNList $ \(Connect aBNIp aBNPort) -> do
        void . C.forkIO $ runClient (showHostAddress aBNIp) (fromEnum aBNPort) "/" $ \aConnect -> do
            WS.sendTextData aConnect . encode $ ActionAddToConnectList aPortNumber
    aConnectLoop aBNList
  where
    aRequestOfPotencialConnects = \case -- IDEA: add random to BN list
        (Connect aBNIp aBNPort):aTailOfList -> do
            aPotencialConnectNumber <- takeRecords aConnectsChan NumberOfRecords
            when (aPotencialConnectNumber == 0) $ do
                void . C.forkIO $ runClient (showHostAddress aBNIp) (fromEnum aBNPort) "/" $ \aConnect -> do
                    WS.sendTextData aConnect $ encode $ RequestPotentialConnects False
                    aMsg <- WS.receiveData aConnect
                    let ResponsePotentialConnects aConnects = fromJust $ decode aMsg
                    writeInChan aConnectsChan $ AddRecords aConnects
                C.threadDelay sec
                aRequestOfPotencialConnects (aTailOfList ++ [Connect aBNIp aBNPort])
        _       -> return ()

    aConnectLoop aBootNodeList  = do
        aActualConnects <- takeRecords aManagerChan ActualConnectsToNNRequest
        if null aActualConnects then do
            aNumberOfConnects <- takeRecords aConnectsChan NumberOfRecords
            when (aNumberOfConnects == 0) $ aRequestOfPotencialConnects aBootNodeList
            aConnects <- takeRecords aConnectsChan ReadRecords
            forM_ aConnects (connectToNN aConnectsChan aMyNodeId inChanPending aInfoChan aManagerChan (fst aSyncChan) aInLogerChan)
            C.threadDelay $ 2 * sec
            aConnectLoop aBootNodeList
        else do
            C.threadDelay $ 10 * sec
            aConnectLoop aBootNodeList


connectToNN aFileServerChan aMyNodeId inChanPending aInfoChan ch aSync aInLogerChan aConn@(Connect aIp aPort)  = do
    writeLog aInfoChan [NetLvlTag] Info $ "Try connecting to: "  ++ showHostAddress aIp
    aOk <- try $ runClient (showHostAddress aIp) (fromEnum aPort) "/" $ \aConnect -> do
        writeLog aInfoChan [NetLvlTag] Info $ "Connecting to: "  ++ showHostAddress aIp
        WS.sendTextData aConnect . A.encode $ ActionConnect NN (Just aMyNodeId)
        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right (ActionConnect aNodeType (Just aNodeId))
                | aMyNodeId /= aNodeId -> do
                    (aInpChan, aOutChan) <- newChan 64
                    sendActionToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing
                    void $ C.forkIO $ do
                        C.threadDelay sec
                        writeInChan aSync RestartSync
                    void $ race
                        (msgSender ch aMyNodeId aConnect aOutChan)
                        (msgReceiver ch aInfoChan aFileServerChan NN (IdFrom aNodeId) aConnect inChanPending aInLogerChan)
            _ -> return ()

    case aOk of
        Left (_ :: SomeException) ->
            void $ tryWriteChan aFileServerChan $ DeleteRecords aConn
        _ -> return ()

--
sec :: Int
sec = 1000000
