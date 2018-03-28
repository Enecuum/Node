{-# LANGUAGE
    LambdaCase,
    ViewPatterns,
    MultiWayIf,
    ScopedTypeVariables
  #-}

module Node.Node.Base where

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


loging :: NodeConfigClass aData => aData -> String -> IO ()
loging aData aString = do
    aTime <- getTime Realtime
    let MyNodeId aNodeId = aData^.myNodeId
    appendFile
        ("./data/log_" ++ show aNodeId ++ "_.txt")
        ("["++ show aTime ++ "] " ++ aString ++ "\n")


baseNodeOpts :: (ManagerData md2, ManagerData md1, ManagerMsg msg) =>
    Chan msg
    -> IORef md1
    -> md2
    -> Options msg ()
baseNodeOpts aChan aMd aData = do
    opt isSendInitDatagram  $ answerToSendInitDatagram aChan aMd
    opt isServerIsDead          $
        answerToServerDead aChan defaultServerPort
    opt isConnectivityQuery     $ answerToConnectivityQuery aChan aMd
    opt isSendDatagram          $ answerToSendDatagram      aMd
    opt isDisconnectNode        $ answerToDisconnectNode    aData
    opt isStateRequest          $ answerToStateRequest      aData
    opt isDeleteDeadSouls       $ answerToDeleteDeadSouls   aData


answerToStateRequest :: (ManagerData md, ManagerMsg msg) => md -> msg -> IO ()
answerToStateRequest aMd _ = do
    let aBroadcastNum = length $ filter (\aNode -> aNode^.status == Active) $
            getNodes BroadcastNode aMd
    writeChan (aMd^.answerChan) $ StateRequestAnswer
        (aMd^.nodeConfig.nodeVariantRoles) (aMd^.myNodeId)

        (aMd^.nodes.to (length . M.keys . M.filter (\a -> a^.status == Active)))
        aBroadcastNum
        (aMd^.nodes.to (length . M.keys))


answerToSendDatagram :: (ManagerData md, ManagerMsg msg) =>
    IORef md -> msg -> IO ()
answerToSendDatagram aMd (toManagerMsg -> SendDatagram aMsg aId) = do
    aData <- readIORef aMd
    whenJust (aId `M.lookup`(aData^.nodes)) $
        \aNode -> sendDatagramFunc (aNode^.chan) aMsg
answerToSendDatagram _ _ = pure ()


sendExitMsgToNode :: Node -> IO ()
sendExitMsgToNode aNode = if
    | Just aKey <- aNode^.mKey -> if
        | Just aMsg <- maybeCryptoError $
            makePackagedMsg (encodePackage (Disconnect [])) aKey -> do
                writeChan (aNode^.chan) (SenderExit $ encode aMsg)
        | otherwise -> do
            writeChan (aNode^.chan) SenderTerminate
    | otherwise -> writeChan (aNode^.chan) SenderTerminate


answerToDeleteDeadSouls :: (ManagerData md, ManagerMsg msg) => md -> msg -> IO ()
answerToDeleteDeadSouls aData _ = do
    let aNodes = filter (\aNode -> aNode^.status /= Active) $ M.elems $ aData^.nodes
    forM_ aNodes sendExitMsgToNode


answerToConnectivityQuery :: (ManagerData md, ManagerMsg msg) =>
        Chan msg -> IORef md -> msg -> IO ()
answerToConnectivityQuery aChan aMd _ = do
    aData <- readIORef aMd
    let aBroadcastNodes = getNodes BroadcastNode aData
        aBroadcastNum   = length $ filter (\aNode -> aNode^.status == Active) $
            aBroadcastNodes
        aConnectingNum = M.size (aData^.nodes) - length (getNodes Active aData)
    let aConnects = BI.elems $ aData^.vacantPositions
    if
        | aBroadcastNum > 4 -> do
            let ns = drop 2 $ sortOn (^.pingTime) aBroadcastNodes
            aShuffledNS <- shuffleM ns
            forM_ (drop 1 aShuffledNS) sendExitMsgToNode

        | aBroadcastNum > 1 || aConnectingNum /= 0 -> return ()

        | not $ iIsBroadcastNode aData -> do
            aListOfConnects <- readRecordFromNodeListFile $ aData^.myNodeId
            if  | null $ aListOfConnects -> connectToBootNode aChan aData
                | otherwise -> connectToListOfConnect
                    aChan (3 - aBroadcastNum) aListOfConnects
        | not $ null aConnects -> do
            connectTo aChan (3 - aBroadcastNum) aConnects
        | Just aIp <- aData^.nodeBaseData.hostAddress, aBroadcastNum > 0 -> do
            sendIHaveBroadcastConnects aMd aIp
        | aBroadcastNum > 0 -> do
            let aNode = head aBroadcastNodes
            aIPRequest <- makeIPRequest
                (keyToId $ aNode^.nPublicKey)
                (aData^.privateKey)
            sendToNode (makePingPongMsg Ping aIPRequest) aNode
        | otherwise -> do
            aListOfConnects <- readRecordFromNodeListFile $ aData^.myNodeId
            if  | null aListOfConnects  -> do
                    connectToBootNode aChan aData
                | otherwise             -> do
                    connectToListOfConnect aChan 2 aListOfConnects

connectToListOfConnect :: ManagerMsg msg =>
    Chan msg -> Int -> [(NodeId, HostAddress, PortNumber)] -> IO ()
connectToListOfConnect aChan aNum aConnects = do
    aShuffledConnects <- shuffleM aConnects
    forM_ (take aNum aShuffledConnects) $ \(aNodeId, aIp, aPort) -> do
        writeChan aChan $ sendInitDatagram aIp aPort aNodeId

connectToBootNode :: (ManagerMsg msg, ManagerData md) => Chan msg -> md -> IO ()
connectToBootNode aChan aData =
    connectToListOfConnect aChan 1 $ aData^.nodeBaseData.bootNodes

connectTo :: ManagerMsg msg =>
    Chan msg -> Int -> [(NodeId, HostAddress, PortNumber)] -> IO ()
connectTo aChan aBroadcastNum = connectToListOfConnect aChan aBroadcastNum

sendIHaveBroadcastConnects :: ManagerData md => IORef md -> HostAddress -> IO ()
sendIHaveBroadcastConnects aMd aIp = do
    aData <- readIORef aMd
    loging aData $ "sendIHaveBroadcastConnects"
    let aBroadcastNum = length $ filter (\aNode -> aNode^.status == Active) $
            getNodes BroadcastNode aData
    aMsg <- makeIHaveBroadcastConnects
        aBroadcastNum
        aIp
        (aData^.nodeConfig.portNumber)
        (aData^.nodeConfig.myNodeId)
        (aData^.nodeConfig.privateKey)
    sendInfoPingToNodes aMd aMsg

iIsBroadcastNode :: ManagerData md => md -> Bool
iIsBroadcastNode aData = BroadcastNode `elem` aData^.nodeConfig.helloMsg.nodeVariantRoles

answerToClientDisconnected ::
    (ManagerData md, ManagerMsg msg) =>
    IORef md -> msg -> IO ()
answerToClientDisconnected aMd (toManagerMsg -> ClientIsDisconnected aId aChan) = do
    aData <- readIORef aMd
    whenJust (aId `M.lookup` (aData^.nodes)) $ \aNode -> do
        when (aNode^.chan == aChan) $ modifyIORef aMd (nodes %~ M.delete aId)
answerToClientDisconnected _ _ = pure ()

answerToSendInitDatagram :: (ManagerData md, ManagerMsg msg) =>
    Chan msg
    -> IORef md
    -> msg
    -> IO ()
answerToSendInitDatagram
    aChan
    aMd
    (toManagerMsg -> SendInitDatagram aIp aPort aId)
    = do
        aData <- readIORef aMd
        loging aData "answerToSendInitDatagram"
        unless (aId `M.member` (aData^.nodes)) $
            sendInitDatagramFunc aChan aIp aPort aId aMd

answerToSendInitDatagram _ _ _ = pure ()

answerToDisconnectNode :: (ManagerData md, ManagerMsg msg) =>
    md
    -> msg
    -> IO ()
answerToDisconnectNode aData (toManagerMsg -> DisconnectNode aId) = do
    loging aData "answerToDisconnectNode"
    whenJust (aId `M.lookup`(aData^.nodes)) $ sendExitMsgToNode

answerToDisconnectNode _ _ = pure ()

sendInitDatagramFunc :: (ManagerMsg a, ManagerData md) =>
    Chan a ->  HostAddress -> PortNumber -> NodeId -> IORef md -> IO ()
sendInitDatagramFunc aManagerChan receiverIp receiverPort aId aMd = do
    aData <- readIORef aMd
    loging aData $ "sendInitDatagramFunc: " ++
        showHostAddress receiverIp ++ ":" ++ show receiverPort ++ " " ++
        show aId

    aMsg <- makeConnectingMsg (aData^.myNodeId)
        (aData^.publicPoint) (aData^.privateKey) (aData^.publicKey)
    aNodeChan <- initSenderSocket aManagerChan receiverIp receiverPort aId aMd
    sendPackagedMsg aNodeChan aMsg
    modifyIORef aMd $ nodes %~ M.adjust (status .~ NodeStatus Remote Auth) aId

answerToServerDead ::
    ManagerMsg a => Chan a -> PortNumber ->  a -> IO ()
answerToServerDead aChan aPort _ =  void $ startServerActor aChan aPort


answerToInitDatagram :: (ManagerData md, ManagerMsg msg) =>
    IORef md -> msg -> IO ()
answerToInitDatagram aMd
    (toManagerMsg -> InitDatagram aInputChan aHostAdress aDatagram) = do
    modifyIORef aMd $
        nodeConfig.helloMsg.nodeVariantRoles %~ lInsert BroadcastNode
    case decode aDatagram of
        Right (conMsg @(ConnectingMsg aMsg aId _ _))
            | verifyConnectingMsg conMsg ->
                answerToInitiatorConnectingMsg aId aHostAdress aInputChan aMsg aMd
        _                               -> writeChan aInputChan SenderTerminate
answerToInitDatagram _ _                =  pure ()


answerToDatagramMsg :: (ManagerData md, ManagerMsg a) =>
    t
    -> IORef md
    -> p
    -> (t -> IORef md -> NodeId -> PingPackage -> IO ())
    -> (t -> IORef md -> NodeId -> PongPackage -> IO ())
    -> (t -> IORef md -> NodeId -> InfoPingPackage -> IO())
    -> a
    -> IO ()
answerToDatagramMsg aChan aMd _ aAnswerToPing aAnswerToPong aAnswerToInfoPing
    (toManagerMsg -> DatagramMsg aDatagramMsg aId) = case decode aDatagramMsg of
        Right (conMsg @(ConnectingMsg aMsg _ _ _))
            | verifyConnectingMsg conMsg -> answerToRemoteConnectingMsg aId aMsg aMd
        Right aPackagedMsg -> answerToPackagedMsg aId
            aChan aPackagedMsg aAnswerToPing aAnswerToPong aAnswerToInfoPing aMd
        _                           -> pure ()
answerToDatagramMsg _ _ _ _ _ _ _    =  pure ()

answerToPackagedMsg :: ManagerData md =>
    NodeId
    -> t
    -> PackagedMsg
    -> (t -> IORef md -> NodeId -> PingPackage -> IO ())
    -> (t -> IORef md -> NodeId -> PongPackage -> IO ())
    -> (t -> IORef md -> NodeId -> InfoPingPackage -> IO())
    -> IORef md
    -> IO ()
answerToPackagedMsg
    aId
    aChan
    aPackagedMsg@(PackagedMsg aEncriptedMsg)
    aAnswerToPing
    aAnswerToPong
    aAnswerToInfoPing
    aMd
    = do
        aData <- readIORef aMd
        loging aData $ "answerToPackagedMsg: " ++ show aEncriptedMsg
        let encodedMsg = do
                key  <- nodeKey =<< (aId `M.lookup`(aData^.nodes))
                maybeCryptoError $ getMsgPackage key aPackagedMsg
        whenJust encodedMsg $ \justEncodedMsg -> do
            loging aData $ "aBinaryMsg from " ++ show aId ++ ": "  ++ show justEncodedMsg
            let aMaybeDecodeMsg = decodePackage justEncodedMsg
            whenLeft aData aMaybeDecodeMsg -- log
            whenRight aMaybeDecodeMsg $ \aDecodeMsg -> do
                loging aData $ "aDecodeMsg: " ++ show aDecodeMsg
                case aDecodeMsg of
                    Hello      aMsg      -> answerToHelloMsg aMsg aId aMd
                    Disconnect aMsg      -> answerToDisconnect aMsg aId aMd
                    Ping p               -> aAnswerToPing aChan aMd aId p
                    Pong p               -> aAnswerToPong aChan aMd aId p
                    InfoPing p           -> aAnswerToInfoPing aChan aMd aId p
answerToPackagedMsg _ _ _ _ _ _ _ = return ()


whenLeft :: (Show a, Show b, NodeConfigClass aData) =>
                 aData -> Either a b -> IO ()
whenLeft aData aMsg@(Left _) = loging aData $ show aMsg
whenLeft _ _ = pure ()

answerToHelloMsg :: ManagerData md => HelloMsg -> NodeId -> IORef md -> IO ()
answerToHelloMsg aMsg aId aMd = do
    aData <- readIORef aMd
    loging aData "answerToHelloMsg"
    case getStatus aId aData of
        Just (NodeStatus Initiator Auth) -> sendHelloDatagram aId aMd
        _                                -> pure ()
    time  <- getTime Realtime

    when (aId `M.member` (aData^.nodes)) $  do
        loging aData $ "Connected: " ++ show (aData^.myNodeId) ++ " to " ++ show aId
    modifyIORef aMd $ nodes %~ M.adjust (&~ do
        mHelloMsg       .= Just aMsg
        status          .= Active

        mark <- use pingMark
        pingTime        .= diffTimeSpec mark time
      ) aId
    whenJust (aId `M.lookup` (aData^.nodes)) $ \aNode -> do
        sendToNode (makePingPongMsg Ping BroadcastNodeListRequest) aNode


answerToDisconnect :: ManagerData md => [Reason] -> NodeId -> IORef md -> IO ()
answerToDisconnect _ aNodeId aMd = do
    aData <- readIORef aMd
    loging aData "answerToDisconnect"
    whenJust (aNodeId `M.lookup` (aData^.nodes)) sendExitMsgToNode


answerToInitiatorConnectingMsg :: (ManagerData md) =>
    NodeId -> HostAddress -> Chan MsgToSender  -> PublicPoint -> IORef md -> IO ()
answerToInitiatorConnectingMsg aId aHostAdress aInputChan aPublicPoint aMd = do
    aData <- readIORef aMd
    loging aData $ "answerToInitiatorConnectingMsg from " ++ showHostAddress aHostAdress ++ " " ++ show aId
    if aId `M.member` (aData^.nodes) then do
        loging aData $ "is refused " ++ showHostAddress aHostAdress ++ " " ++ show aId
        writeChan aInputChan SenderTerminate
    else do
        loging aData $ "is accepted " ++ showHostAddress aHostAdress ++ " " ++ show aId
        let initSoketAndSendMsg = do
                modifyIORef aMd $ nodes %~ M.insert aId (makeNode aInputChan aId aHostAdress)
                aNewData <- readIORef aMd
                sendRemoteConnectDatagram
                    (fromJust (aId `M.lookup` (aNewData^.nodes)) ^.chan) aNewData
                modifyIORef aMd $ nodes %~ M.adjust (&~ do
                    mPublicPoint    .= Just aPublicPoint
                    mKey            .= Just (getKay (aNewData^.privateNumber) aPublicPoint)
                    status          .= NodeStatus Initiator Auth
                  ) aId
        case getStatus aId aData of
            Nothing       -> initSoketAndSendMsg
            Just Noactive -> initSoketAndSendMsg
            _             -> pure ()


answerToRemoteConnectingMsg :: ManagerData md =>
    NodeId -> PublicPoint -> IORef md -> IO ()
answerToRemoteConnectingMsg aId aPublicPoint aMd = do
    aData <- readIORef aMd
    loging aData $ "answerToRemoteConnectingMsg from " ++ show aId
    case getStatus aId aData of
        Just (NodeStatus Remote Auth) -> do
            time  <- getTime Realtime
            modifyIORef aMd $ nodes %~ M.adjust (&~ do
                mPublicPoint    .= Just aPublicPoint
                mKey            .= Just (getKay (aData^.privateNumber) aPublicPoint)
                status          .= NodeStatus Remote Auth
                pingMark        .= time
              ) aId
            sendHelloDatagram aId aMd
        _   -> pure ()

sendRemoteConnectDatagram ::
    ManagerData md => Chan MsgToSender -> md -> IO ()
sendRemoteConnectDatagram aChan aData = do
    loging aData $ "sendRemoteConnectDatagram"
    aMsg <- makeConnectingMsg (aData^.myNodeId)
        (aData^.publicPoint) (aData^.privateKey)  (aData^.publicKey)
    sendPackagedMsg aChan aMsg

{-# DEPRECATED sendDatagramFunc "Use sendPackagedMsg" #-}
sendDatagramFunc :: Chan MsgToSender -> B.ByteString -> IO ()
sendDatagramFunc aChan aMsg = writeChan aChan $ MsgToSender aMsg


sendPackagedMsg :: Chan MsgToSender -> PackagedMsg -> IO ()
sendPackagedMsg aChan aMsg = sendDatagramFunc aChan $ encode aMsg

sendHelloDatagram :: ManagerData md =>
    NodeId -> IORef md -> IO ()
sendHelloDatagram aId aMd = readIORef aMd >>= \aData -> do
    loging aData $ "sendHelloDatagram to " ++ show aId
    sendJustPackagedMsg $ makeMsg aId aData $ makePackagedMsg
        (encodePackage $ Hello $ aData^.nodeConfig.helloMsg)


initSenderSocket :: (ManagerMsg a, ManagerData md) =>
    Chan a -> HostAddress -> PortNumber -> NodeId -> IORef md -> IO (Chan MsgToSender)
initSenderSocket aManagerChan aIp aPort aId aMd = do
    aData <- readIORef aMd
    loging aData $ "initSenderSocket to " ++ showHostAddress aIp ++ ":"
        ++ show aPort ++ " " ++ show aId
    aNodeChan  <- initSender aId aManagerChan aIp aPort
    let fAlter = \case
          Just lNode    -> Just $ lNode & chan .~ aNodeChan
          _             -> Just $ makeNode aNodeChan aId aIp
    modifyIORef aMd (nodes %~ M.alter fAlter aId)
    return aNodeChan


startServerActor :: ManagerMsg a => Chan a -> PortNumber -> IO ()
startServerActor aOutputChan aPort = do
    void $ forkIO $ runServer 0 (fromEnum aPort) $
        \aHostAdress pending -> do
            conn <- WS.acceptRequest pending
            WS.forkPingThread conn 30
            aMsg <- WS.receiveData conn
            case decode aMsg of
                Right (conMsg@(ConnectingMsg _ aId _ _))
                    | verifyConnectingMsg conMsg -> do
                            aInputChan <- newChan
                            writeChan aOutputChan $
                                initDatagram aInputChan aHostAdress aMsg
                            socketActor aHostAdress aId aOutputChan aInputChan conn
                _     -> pure ()


initSender :: ManagerMsg a =>
    NodeId -> Chan a -> HostAddress -> PortNumber -> IO (Chan MsgToSender)
initSender aId aChan aIp aPort = do
    aInputChan <- newChan
    void $ forkIO $ runClient
        (showHostAddress aIp)
        (fromEnum aPort) "/"
        (socketActor aIp aId aChan aInputChan)
                `finally` (writeChan aChan $ clientIsDisconnected aId aInputChan)
    return aInputChan


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


type PingAnswer a c = Chan c -> IORef a -> NodeId -> PingPackage -> IO ()
type PongAnswer a c = Chan c -> IORef a -> NodeId -> PongPackage -> IO ()
type InfoPingAnswer a c =
    Chan c -> IORef a -> NodeId -> InfoPingPackage -> IO ()


answerToPong :: (ManagerData md, ManagerMsg msg) => PongAnswer md msg
answerToPong _ _ _ _ = return ()

minusStatusNumber :: (NodeBaseDataClass a, NodeConfigClass a) =>
    IORef a -> NodeId -> IO ()
minusStatusNumber aMd aId = do
    aData <- readIORef aMd
    whenJust (aId `M.lookup` (aData^.nodes)) $ \_ -> do
        loging aData $ "Disconnected: " <> show aId


sendJustPackagedMsg :: Maybe (Chan MsgToSender, PackagedMsg) -> IO ()
sendJustPackagedMsg x = whenJust x $ uncurry sendPackagedMsg

sendJustDatagram :: Maybe (Chan MsgToSender, PackagedMsg) -> IO ()
sendJustDatagram = sendJustPackagedMsg


makeMsg :: ManagerData s =>
    NodeId -> s -> (StringKey -> CryptoFailable b) ->
    Maybe (Chan MsgToSender, b)
makeMsg aId aData func = do
    aNode        <- aId `M.lookup` (aData^.nodes)
    packagedMsg  <- maybeCryptoError . func =<< aNode^.mKey
    pure (nodeChan aNode, packagedMsg)

getStatus :: ManagerData md => NodeId -> md -> Maybe NodeStatus
getStatus aId aMd = (^.status) <$> (aId `M.lookup` (aMd^.nodes))


class GetNodes a where
    getNodes :: ManagerData md => a -> md -> [Node]


instance GetNodes NodeVariantRole where
    getNodes aRole aData = filter (\n -> (do
        aMsg <- n^.mHelloMsg
        pure $ aRole `elem` aMsg^.nodeVariantRoles) == Just True) $
            aData^.nodes.to M.elems

instance GetNodes NodeStatus where
    getNodes aStatus aData = filter (\aNode -> aNode^.status == aStatus) $
        aData^.nodes.to M.elems


sendInfoPingToNodes :: ManagerData md => IORef md -> InfoPingPackage -> IO ()
sendInfoPingToNodes aMd aInfoPing = do
    aData <- readIORef aMd
    sendToNodes aData aMakeMsg
  where
    aMakeMsg :: StringKey -> CryptoFailable PackagedMsg
    aMakeMsg aKey = makePingPongMsg InfoPing aInfoPing aKey


sendToNodes :: ManagerData md =>
    md -> (StringKey -> CryptoFailable PackagedMsg) -> IO ()
sendToNodes aData aMakeMsg = do
    forM_ (getNodes BroadcastNode aData <> getNodes SimpleNode aData)
        (sendToNode aMakeMsg)

sendToNode :: (StringKey -> CryptoFailable PackagedMsg) -> Node -> IO ()
sendToNode aMakeMsg aNode = do
    whenJust (aNode^.mKey) $ \aKey -> do
        whenJust (maybeCryptoError $ aMakeMsg aKey) $ \aJustMsg -> do
            sendPackagedMsg (aNode^.chan) aJustMsg



readRecordFromNodeListFile :: MyNodeId -> IO [(NodeId, HostAddress, PortNumber)]
readRecordFromNodeListFile (MyNodeId aMyNodeId) = do
    aFileContent <- readDataFile $
        "./data/listOfConnects" ++ show aMyNodeId ++ ".txt"
    forM aFileContent $ \(aNodeId, aIp, aPort) ->
        return (NodeId aNodeId, aIp, aPort)


addRecordToNodeListFile :: MyNodeId -> NodeId -> HostAddress -> PortNumber -> IO ()
addRecordToNodeListFile (MyNodeId aMyNodeId) (NodeId aNodeId) aIp aPort = do
    unless (aMyNodeId == aNodeId) $ do
        aFileContent <- readRecordFromNodeListFile (MyNodeId aMyNodeId)
        unless ((NodeId aNodeId, aIp, aPort)`elem`aFileContent) $
            addDataToFile
                ("./data/listOfConnects" ++ show aMyNodeId ++ ".txt")
                (aNodeId, aIp, aPort)


lInsert :: NodeVariantRole -> [NodeVariantRole] -> [NodeVariantRole]
lInsert _ [BootNode] = [BootNode]
lInsert aElem aList  = S.toList . S.fromList $ aElem : aList
