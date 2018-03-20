{-# LANGUAGE
    OverloadedStrings,
    MultiWayIf,
    ViewPatterns,
    StandaloneDeriving,
    TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Node.Node.Mining where

import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA

import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import              Service.Types.PublicPrivateKeyPair
import              Data.List.Extra
import              Service.Types
import              System.Clock
import              System.Random.Shuffle
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Control.Concurrent.Chan
import              Control.Monad.Extra
import              Node.Node.Base
import              Node.Node.Types
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Service.Metrics


managerMining :: Chan ManagerMiningMsgBase -> IORef ManagerNodeData -> IO ()
managerMining ch aMd = forever $ do
    mData <- readIORef aMd
    readChan ch >>= \a -> runOption a $ do
        baseNodeOpts ch aMd mData

        opt isInitDatagram          $ answerToInitDatagram aMd
        opt isDatagramMsg           $ answerToDatagramMsg ch aMd (mData^.myNodeId)
            miningNodeAnswerToPing
            miningNodeAnswerToPong
            miningNodeAnswerToInfoPing

        opt isClientIsDisconnected $
                miningNodeAnswerClientIsDisconnected aMd

        opt isNewTransaction            $ answerToNewTransaction aMd
        opt isInitDatagram              $ answerToSendInitDatagram ch aMd
        opt isBlockMadeMsg              $ answerToBlockMadeMsg aMd
        opt isDeleteOldestMsg           $ answerToDeleteOldestMsg aMd
        opt isSendTargetedTransaction   $ answerToSendTargetedTransaction aMd
        opt isSendIAmPublicator         $ answerToSendIAmPublicator aMd
        opt isSendRawData               $ answerToSendRawData aMd
        opt isResendTransactionToPublicator $ answerToResendTransactionToPublicator aMd
        opt isSendTransactionToPublicator $ answerToSendTransactionToPublicator aMd
        opt isDeleteOldestVacantPositions $ answerToDeleteOldestVacantPositions aMd

miningNodeAnswerClientIsDisconnected ::
    IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
miningNodeAnswerClientIsDisconnected aMd
    (toManagerMsg -> ClientIsDisconnected aNodeId aChan) = do
        aData <- readIORef aMd
        loging aData $ "miningNodeAnswerClientIsDisconnected " ++ show aNodeId
        whenJust (aNodeId `M.lookup`(aData^.nodes)) $ \aNode -> do
            when (aChan == (aNode^.chan)) $ do
                minusStatusNumber aMd aNodeId
                modifyIORef aMd $ (nodes %~ M.delete aNodeId)
miningNodeAnswerClientIsDisconnected _ _ = pure ()


answerToDeleteOldestVacantPositions ::
    IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToDeleteOldestVacantPositions aMd _ = do
    aTime <- getTime Realtime
    modifyIORef aMd $ vacantPositions %~ BI.filter (\aTimeSpec _ ->
        diffTimeSpec aTime aTimeSpec > 3000000)

answerToDeleteOldestMsg :: IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToDeleteOldestMsg aMd _ = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ deleteOldest aTime

miningNodeAnswerToPong :: PongAnswer ManagerNodeData ManagerMiningMsgBase
miningNodeAnswerToPong _ aMd aId aPong = do
    aData <- readIORef aMd
    loging aData $ "miningNodeAnswerToPong " ++ show aPong
    case aPong of
        aIPAnswer@(IPAnswer aIp aTimeSpec _)
            | verifyIPAnswer aId (aData^.publicKey) aIPAnswer -> do
                aTime <- getTime Realtime
                when (diffTimeSpec aTime aTimeSpec < 3000000000) $
                    modifyIORef aMd $ nodeBaseData.hostAddress .~ Just aIp

        BroadcastNodeListAnswer aListAnswer ->
            forM_ aListAnswer $ \(aNodeId, (aIp, aPort)) ->
                addRecordToNodeListFile (aData^.myNodeId) aNodeId aIp aPort
        _                                   -> pure ()

miningNodeAnswerToPing ::  PingAnswer ManagerNodeData ManagerMiningMsgBase
miningNodeAnswerToPing _ aMd aNodeId aPing = do
    aData <- readIORef aMd
    loging aData $ "miningNodeAnswerToPing" ++ show aPing
    case aPing of
        IPRequest aTimeSpec aSignature -> whenJust (aNodeId `M.lookup` (aData^.nodes)) $
            \aNode -> sendToNode
                (makePongMsg (IPAnswer (aNode^.nHostAddress) aTimeSpec aSignature))
                aNode

        BroadcastNodeListRequest -> whenJust (aNodeId `M.lookup` (aData^.nodes)) $
            \aNode -> do
                aRawListOfContacts <- readRecordFromNodeListFile $ aData^.myNodeId
                aListOfContacts    <- shuffleM $ map (\(a, b, c) -> (a, (b, c)))
                    aRawListOfContacts
                sendToNode
                    (makePongMsg (BroadcastNodeListAnswer $ take 5 aListOfContacts))
                    aNode
        _ -> return ()

miningNodeAnswerToInfoPing :: InfoPingAnswer  ManagerNodeData ManagerMiningMsgBase
miningNodeAnswerToInfoPing _ aMd _ aInfoPing = do
    aData <- readIORef aMd
    loging aData $ "miningNodeAnswerToInfoPing " ++ show aInfoPing
    case aInfoPing of
        RoamTransaction aTransaction -> sendTransactionToRandomPublicator
            aMd aTransaction

        _ -> when (notInIndex aData aInfoPing) $ do
            addInIndex aInfoPing aMd
            sendInfoPingToNodes aMd aInfoPing
            processingOfInfoPing aMd aInfoPing

acceptTransactionAndConfirmation ::
    Transaction
    -> IORef ManagerNodeData
    -> IO ()
acceptTransactionAndConfirmation  aTransaction aMd = do
    aData <- readIORef aMd
    loging aData $ "acceptTransactionAndConfirmation " ++ show aTransaction
    metric $ add
        ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount")
        (1 :: Integer)
    writeChan (aData^.transactions) aTransaction
    aTransactionConfirmation <- makeTransactionConfirmation aTransaction
        (aData^.myNodeId) (aData^.privateKey)
    sendToNodes aData (makeInfoPing aTransactionConfirmation)

answerToSendRawData :: IORef ManagerNodeData
    -> ManagerMiningMsgBase
    -> IO ()
answerToSendRawData aMd (SendRawData aMsg) = do
    metric $ increment "net.bl.count"
    sendRawDataToNodes aMd aMsg
answerToSendRawData _ _ = error "answerToSendRawData: something unexpected  has happened"

answerToSendTargetedTransaction ::
    IORef ManagerNodeData
    -> ManagerMiningMsgBase
    -> IO ()
answerToSendTargetedTransaction aMd
    (SendTargetedTransaction aTransaction aNid) =
        sendInfoPingToNodes aMd $ NewTargetedTransaction aTransaction aNid
answerToSendTargetedTransaction _ _ = error
    "answerToSendTargetedTransaction: something unexpected  has happened."

answerToSendIAmPublicator ::
    IORef ManagerNodeData
    -> ManagerMiningMsgBase
    -> IO ()
answerToSendIAmPublicator aMd _ =  do
    aData <- readIORef aMd
    aTime <- getTime Realtime
    infoPingIAmPublicator <- makeInfoPingIAmPublicator
        (aData^.myNodeId) aTime (aData^.privateKey)
    sendInfoPingToNodes aMd infoPingIAmPublicator

answerToResendTransactionToPublicator ::
    IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToResendTransactionToPublicator aMd _ = do
    aData <- readIORef aMd
    aTime <- getTime Realtime
    let ((_, aTransaction), aNewMap) = BI.deleteFindMin $ aData^.sendedTransctions
    modifyIORef aMd $ sendedTransctions .~ BI.insert aTime aTransaction aNewMap
    sendTransactionToRandomPublicator aMd aTransaction

answerToNewTransaction :: IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToNewTransaction aMd (NewTransaction aTransaction) = do
    metric $ increment "net.tx.count"
    aData <- readIORef aMd
    loging aData $ "new transaction in net"
    addInIndex (NewTransactionInNet aTransaction) aMd
    sendInfoPingToNodes aMd $ NewTransactionInNet aTransaction
    metric $ add
        ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount")
        (1 :: Integer)
    writeChan (aData^.transactions) aTransaction
answerToNewTransaction _ _ = error
    "answerToNewTransaction: something unexpected  has happened."


answerToBlockMadeMsg :: ManagerMiningMsg msg =>
    IORef ManagerNodeData -> msg -> IO ()
answerToBlockMadeMsg aMd (toManagerMiningMsg -> BlockMadeMsg aMicroblock) = do
    metric $ increment "net.bl.count"
    aData <- readIORef aMd
    loging aData $ "answerToBlockMadeMsg " ++ show aMicroblock

    addInIndex (BlockMade aMicroblock) aMd
    sendInfoPingToNodes aMd $ BlockMade aMicroblock
    writeChan (aData^.microblockChan) aMicroblock
answerToBlockMadeMsg _ _ = pure ()

deriving instance Ord Transaction
deriving instance Ord Signature

answerToSendTransactionToPublicator ::
    IORef ManagerNodeData
    -> ManagerMiningMsgBase
    -> IO ()
answerToSendTransactionToPublicator aMd (SendTransactionToPublicator aTransaction) = do
    aTime <- getTime Realtime
    modifyIORef aMd (sendedTransctions %~ BI.insert aTime aTransaction)
    sendTransactionToRandomPublicator aMd aTransaction
answerToSendTransactionToPublicator _ _ = error
    "answerToSendTransactionToPublicator: something unexpected  has happened"


verifyNewData :: ManagerData md =>
    ECDSA.Signature -> NodeId -> B.ByteString -> md -> Bool
verifyNewData sig aNodeId bs aMd = if
    | Just k <- key -> verifyByteString  k sig bs
    | otherwise     -> False
  where
    key :: Maybe ECDSA.PublicKey
    key = nodePublicKey <$> (aNodeId `M.lookup` (aMd^.nodes))

isBrodcastNode :: ManagerNodeData -> Bool
isBrodcastNode aData = aData^.nodeConfig.helloMsg.nodeVariantRoles.to
    (BroadcastNode `elem`)

notInIndex :: Serialize a => ManagerNodeData -> a -> Bool
notInIndex aData a = not $ BI.memberR (cryptoHash a) $ aData^.hashMap


addInIndex :: Serialize a => a -> IORef ManagerNodeData -> IO ()
addInIndex aMsg aMd = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.insert aTime (cryptoHash aMsg)

deleteOldest ::
    TimeSpec
    -> BI.Bimap TimeSpec B.ByteString
    -> BI.Bimap TimeSpec B.ByteString
deleteOldest aTime = BI.filter
    (\aOldTime _ -> diffTimeSpec aOldTime aTime < fromNanoSecs 3000000)


isBootNode :: NodeId -> ManagerNodeData -> Bool
isBootNode aId aData = aId `elem`
    ((\(i, _, _) -> i) <$> (aData^.nodeBaseData.bootNodes))


sendRawDataToNodes :: IORef ManagerNodeData -> B.ByteString -> IO ()
sendRawDataToNodes aMd aMsg = do
    aData <- readIORef aMd
    addInIndex aMsg aMd
    sendInfoPingToNodes aMd $ InfoPingRawPackage aMsg
    writeChan (aData^.answerChan) (RawPackege aMsg)


sendTransactionToRandomPublicator :: IORef ManagerNodeData -> Transaction -> IO ()
sendTransactionToRandomPublicator aMd aTransaction = do
    aData <- readIORef aMd
    if
        | PublicatorNode `elem` aData^.nodeConfig.nodeVariantRoles -> do
            acceptTransactionAndConfirmation aTransaction aMd
        | aData^.publicators.to (not . S.null) -> do
            aPublicators <- shuffleM $ S.toList (aData^.publicators)
            sendInfoPingToNodes aMd $
                NewTargetedTransaction aTransaction $ head aPublicators
        | otherwise -> do
            let aNodes = drop 1 $ sortOn (^.pingTime) $ getNodes BroadcastNode aData
            forM_ aNodes $ sendToNode $
                (\aStringKey -> makeInfoPing
                    (RoamTransaction aTransaction) aStringKey)

eq :: MyNodeId -> NodeId -> Bool
eq (MyNodeId aMyNodeId) (NodeId aNodeId) = aMyNodeId == aNodeId

processingOfInfoPing :: IORef ManagerNodeData -> InfoPingPackage -> IO ()
processingOfInfoPing aMd aInfoPing = do
    aData <- readIORef aMd
    loging aData $ "Recived " ++ show aInfoPing
    case aInfoPing of
        NewTargetedTransaction aTransaction aNodeId
            | aData^.myNodeId.to ((==) aNodeId . toNodeId) ->
                acceptTransactionAndConfirmation aTransaction aMd
        InfoPingRawPackage aMsg ->
            writeChan (aData^.answerChan) $ RawPackege aMsg
        NewTransactionInNet aTransaction -> do
            metric $ add
                ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount")
                (1 :: Integer)
            writeChan (aData^.transactions) aTransaction
        aMsg@(IHaveBroadcastConnects _ _ aIp aPort aNodeId _)
            | verifyIHaveBroadcastConnects aMsg ->
                unless ((aData^.myNodeId) `eq` aNodeId) $ do
                    aTime <- getTime Realtime
                    modifyIORef aMd $ vacantPositions %~ BI.insert aTime
                        (aNodeId, aIp, aPort)
                    addRecordToNodeListFile (aData^.myNodeId) aNodeId aIp aPort
        TransactionConfirmation aTransaction _ _ ->
            modifyIORef aMd $ sendedTransctions %~ BI.deleteR aTransaction
        BlockMade aMicroblock -> do
            writeChan (aData^.microblockChan) aMicroblock
        _ -> return ()
