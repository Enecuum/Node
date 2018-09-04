{-# LANGUAGE LambdaCase #-}

module Enecuum.Legacy.Node.SyncServer where

import           Control.Concurrent.Chan.Unagi.Bounded
-- import           Control.Concurrent.MVar
-- import           Control.Monad
import           Data.Aeson                            as A
import           Enecuum.Legacy.Node.Data.GlobalLoging
import           Enecuum.Legacy.Node.Data.Key
import           Enecuum.Legacy.Node.DBActor
import           Enecuum.Legacy.Node.NetLvl.Messages
import           Enecuum.Legacy.Node.Node.Types
import           Enecuum.Legacy.Service.Chan
import           Enecuum.Legacy.Service.Sync.SyncJson
import           Enecuum.Legacy.Service.Timer
import           Enecuum.Legacy.Service.Types
import           Universum

syncServer :: (InChan SyncEvent, OutChan SyncEvent) -> InChan MsgToDB -> InChan MsgToCentralActor -> InChan InfoMsg -> IO b
syncServer (aInputSync, outSyncChan) aDBActorChan aManagerChan aInfoChan = do
    let aLog aMsg = writeLog aInfoChan [SyncTag] Info aMsg
    writeLog aInfoChan [SyncTag, InitTag] Info "Init syncServer"
    metronome 60000000 $ writeInChan aInputSync RestartSync
    forever $ do
      aLog $ "Start of syncServer stap."
      readChan outSyncChan >>= \case
        RestartSync -> do
            aLog "Request of all tails"
            aActualConnects <- takeRecords aManagerChan ActualConnectsToNNRequest
            forM_ aActualConnects $ \(ActualConnectInfo aNodeId aNodeType _) -> do
                when (aNodeType == NN) $ sendMsgToNode aManagerChan RequestTail aNodeId

        SyncMsg aNodeId aMsg -> do
            aLog $ "Received SyncMsg to syncServer: " ++ show aMsg
            let aSend amsg = sendMsgToNode aManagerChan amsg aNodeId
            case aMsg of
                RequestTail                                 -> do
                    aLog "Processing of RequestTail."
                    takeRecords aDBActorChan MyTail >>= \case
                        Just aTail  -> do
                            aLog $ "aTail " ++ show aTail
                            aSend $ ResponseTail $ fst aTail
                        Nothing     -> do
                            aLog "Empty tail #001"
                            aSend $ StatusSyncMessage "Empty tail" "#001"

                ResponseTail aNum -> do
                    aLog $ "Received response tail " ++ show aNum
                    aMyTail  <- takeMyTail aDBActorChan
                    aLog $ "My tail is: " ++ show aMyTail
                    when (aMyTail < aNum) $ do
                        aLog $ "Sending of chain request."
                        aSend RequestChain
                ResponseChain aChunk -> do
                    aLog $ "Response chain" ++ show aChunk
                    writeInChan aDBActorChan (WriteChain aChunk)

                RequestChain ->
                    aSend . ResponseChain =<< takeRecords aDBActorChan GetChain

                _ -> aSend $ StatusSyncMessage ("Send command are not allowed for server")  "#005"


data Response a = Response NodeId a deriving Show
first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c


-- find length of my chain
takeMyTail :: InChan MsgToDB -> IO Number
takeMyTail aDBActorChan =
    takeRecords aDBActorChan MyTail >>= \case
        Just (aNum, _)  -> return aNum
        Nothing         -> return 0


takeTailNum :: Response (Number, a) -> Number
takeTailNum (Response _ (aNum, _)) = aNum


takeRecords :: InChan a -> (MVar p -> a) -> IO p
takeRecords aChan aTaker  = do
    aVar <- newEmptyMVar
    writeInChan aChan $ aTaker aVar
    takeMVar aVar

sendMsgToNode :: ToJSON a => InChan MsgToCentralActor -> a -> NodeId -> IO ()
sendMsgToNode aChan aMsg aId = writeInChan aChan $ SendSyncMsg aId (toJSON aMsg)
