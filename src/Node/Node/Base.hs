{-# LANGUAGE
        LambdaCase
    ,   ViewPatterns
    ,   MultiWayIf
    ,   ScopedTypeVariables
    ,   MultiParamTypeClasses
    ,   FlexibleContexts
    ,   PatternSynonyms
#-}

module Node.Node.Base where

import qualified    Network.WebSockets                  as WS
import              System.Clock
import              System.Random
import              System.Random.Shuffle
import              Control.Monad.State.Lazy
import              Control.Monad.Extra
import              Control.Exception
import              Control.Concurrent
import              Control.Concurrent.Chan
import              Control.Concurrent.Async
import              Crypto.Error
import              Crypto.PubKey.ECC.ECDSA
import qualified    Data.ByteString                 as B
import qualified    Data.Map                        as M
import qualified    Data.Set                        as S
import              Data.IORef
import              Data.Serialize
import              Data.Monoid
import              Lens.Micro.Mtl
import              Lens.Micro

import              Service.Network.WebSockets.Client
import              Service.Network.Base
import              Service.Monad.Option
import              Sharding.Space.Point
import              Sharding.Space.Shift
import              Sharding.Sharding
import              Node.Node.Types
import              Node.Crypto
import              Node.Data.Data
import              Node.FileDB.FileDB
import              Node.Extra
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges
import              Node.Node.Base.Server



loging :: NodeConfigClass aData => aData -> String -> IO ()
loging aData aString = do
    aTime <- getTime Realtime
    let MyNodeId aNodeId = aData^.myNodeId
    appendFile
        ("./data/log_" ++ show aNodeId ++ "_.txt")
        ("["++ show aTime ++ "] " ++ aString ++ "\n")


baseNodeOpts
    ::  ManagerData md2
    =>  ManagerData md1
    =>  ManagerMsg msg
    =>  Chan msg
    ->  IORef md1
    ->  md2
    ->  Options msg ()
baseNodeOpts aChan aMd aData = do
    opt isSendInitDatagram  $ answerToSendInitDatagram aChan aMd
    opt isServerIsDead          $
        answerToServerDead aChan defaultServerPort
    opt isConnectivityQuery     $ answerToConnectivityQuery aChan aMd
    opt isSendDatagram          $ answerToSendDatagram      aMd
    opt isDisconnectNode        $ answerToDisconnectNode    aData
    opt isDeleteDeadSouls       $ answerToDeleteDeadSouls   aData


answerToSendDatagram
    ::  ManagerData md
    =>  ManagerMsg msg
    =>  IORef md
    ->  msg
    ->  IO ()
answerToSendDatagram aMd (toManagerMsg -> SendDatagram aMsg aId) = do
    aData <- readIORef aMd
    whenJust (aId `M.lookup`(aData^.nodes)) $
        \aNode -> sendDatagramFunc (aNode^.chan) aMsg
answerToSendDatagram _ _ = pure ()


sendExitMsgToNode :: Node -> IO ()
sendExitMsgToNode ((^.chan) -> aChan) = do
    sendPackagedMsg aChan disconnectRequest
    writeChan       aChan SenderTerminate


answerToDeleteDeadSouls
    ::  ManagerData md
    =>  ManagerMsg msg
    =>  md
    ->  msg
    ->  IO ()
answerToDeleteDeadSouls aData _ = do
    let aNotIsActive aNode = aNode^.status /= Active
    forM_ (M.filter aNotIsActive $ aData^.nodes) sendExitMsgToNode

pattern Head aId aElem <- (aId, aElem):_
pattern PositionOfFirst aPosition <- Head _ ((^.nodePosition) -> Just aPosition)

preferedBroadcastCount :: Int
preferedBroadcastCount = 4

answerToConnectivityQuery
    ::  ManagerData md
    =>  ManagerMsg msg
    =>  Chan msg
    ->  IORef md
    ->  msg
    ->  IO ()
answerToConnectivityQuery aChan aMd _ = do
    aData <- readIORef aMd
    let aNeighbors    = aData^.nodes
        aBroadcasts   = filter (^._2.isBroadcast) $ M.toList aNeighbors

        aMyNodeId     = aData^.myNodeId
        aBroadcastNum = length aBroadcasts
        aUnActiveNum  = M.size $ M.filter (\a -> a^.status /= Active) aNeighbors

    NodeInfoListNetLvl aConnectList     <- readRecordsFromNodeListFile aMyNodeId
    NodeInfoListLogicLvl aPossitionList <- readRecordsFromNodeListFile aMyNodeId
    let aWait = aBroadcastNum >= preferedBroadcastCount{- || aBroadcastNum <= 6 -} || aUnActiveNum /= 0

        aConnectMap   = M.fromList $ (\(a,b,c) -> (a, (b, c))) <$> aConnectList
        aPossitionMap = M.fromList $ aPossitionList
        aFilteredPositions = S.fromList . M.elems $
            M.intersection aPossitionMap aConnectMap

    if  | aWait             -> return ()
        | null aConnectList -> connectToBootNode aChan aData
        | iDontHaveAPosition aData -> if
            | aBroadcastNum == 0 -> connectTo aChan 1 aConnectList
            | PositionOfFirst aPosition <- aBroadcasts -> do
                aDeltaX <- randomRIO (0, 2000)
                aDeltaY <- randomRIO (0, 2000)
                let aMyNodePosition = MyNodePosition $ Point
                        (x + aDeltaX - 1000) (y + aDeltaY - 1000)
                    NodePosition (Point x y) = aPosition
                aChanOfSharding <- newChan
                makeShardingNode aMyNodeId aChanOfSharding aChan aMyNodePosition
                modifyIORef aMd (&~ do
                    myNodePosition .= Just aMyNodePosition
                    shardingChan   .= Just aChanOfSharding)
            | Head aNodeId aNode <- aBroadcasts -> do
                let aPositionRequest = NodePositionRequestPackage
                aPackageSignature <- makePackageSignature aData aPositionRequest
                let aRequestLogicLvlPackage = RequestLogicLvlPackage
                        aPositionRequest aPackageSignature

                aTraceSignature <- makePackageSignature aData
                    (aNodeId, aRequestLogicLvlPackage)
                let aTraceRouting = ToNode aNodeId aTraceSignature
                    aRequest = PackageTraceRoutingRequest aTraceRouting aRequestLogicLvlPackage

                sendToNode (makeCipheredPackage aRequest) aNode
-- findNearestNeighborPositions :: MyNodePosition -> S.Set NodePosition -> [NodePosition]

        |   aBroadcastNum < preferedBroadcastCount,
            Just aMyNodePosition <- aData^.myNodePosition -> do

            let aPositionOfPreferedConnect = findNearestNeighborPositions
                    aMyNodePosition aFilteredPositions
                isPreferedByPositon a = (a^._2) `elem`aPositionOfPreferedConnect
                aNodesId = (^._1) <$> filter isPreferedByPositon aPossitionList
                isPreferedById a = (a^._1)  `elem` aNodesId
                aPreferedConnects = filter isPreferedById aConnectList
            connectTo aChan (preferedBroadcastCount - aBroadcastNum) aPreferedConnects

            --if we don't find anybody send message error
        -- TODO: optimize by net and logic lvl
      --  | aBroadcastNum > 6     -> undefined
--
--makePositionRequest

iDontHaveAPosition :: ManagerData md => md -> Bool
iDontHaveAPosition aData = aData^.myNodePosition /= Nothing

-- MyNodePosition = Nothing, aBroadcastNum == 0, notNull aListOfConnects

-- Maybe MyNodePosition

-- findNearestNeighborPositions :: MyNodePosition -> S.Set NodePosition -> [NodePosition]

-- is my prefered connects: (1,2,3,4)
-- i have connnects: (1,2,4, 7)




-- ToNetLvl   -> NodePosition
-- ToLogicLvl -> is brodcast?
--
--
--  If i have logic coordinates. (1)
--      aNeighbor <- distance (I, neighbor) -> min.
--      if i don't have the a neighbor -> (2)
--      connect to the aNeighbor
--      reques listOfConnects of the aNeighbor
--      connect in the grid



-- If i don't have logic coordinates

--  x (0,0)  y (100, 100) z(200, 0)
--           y (100, 100) z(200, 0)
--    (-50, -50)
--   x -> y
--


{-

---------------------------
--    let aBroadcastNodes = getNodes BroadcastNode aData
    let
        aBroadcastNum   = length $ filter (\aNode -> aNode^.status == Active) $
            aBroadcastNodes
        aConnectingNum = M.size (aData^.nodes) - length (getNodes Active aData)
    let aConnects = BI.elems $ aData^.vacantPositions
    if
        | aBroadcastNum > 4 -> do
            let ns = drop 2 aBroadcastNodes
            aShuffledNS <- shuffleM ns
            forM_ (drop 1 aShuffledNS) sendExitMsgToNode

        | aBroadcastNum > 1 || aConnectingNum /= 0 -> return ()

        | not $ iIsBroadcastNode aData -> do
            aListOfConnects <- readRecordsFromNodeListFile $ aData^.myNodeId
            if  | null $ aListOfConnects -> connectToBootNode aChan aData
                | otherwise -> connectTo
                    aChan (3 - aBroadcastNum) aListOfConnects
        | not $ null aConnects -> do
            connectTo aChan (3 - aBroadcastNum) aConnects
        | Just aIp <- aData^.nodeBaseData.hostAddress, aBroadcastNum > 0 -> do
            undefined
            --sendIHaveBroadcastConnects aMd aIp
        | aBroadcastNum > 0 -> do
            undefined

        | otherwise -> do
            aListOfConnects <- readRecordsFromNodeListFile $ aData^.myNodeId
            if  | null aListOfConnects  -> do
                    connectToBootNode aChan aData
                | otherwise             -> do
                    connectTo aChan 2 aListOfConnects

-}

{-
connectByPosition
    ::  ManagerMsg msg
    =>  ManagerData md
    =>  Chan msg
    ->  md
    ->  [NodePosition]
    ->  NodeInfoList NetLvl
    ->  NodeInfoList LogicLvl
    ->  IO ()

-}

connectTo
    ::  ManagerMsg msg
    =>  Chan msg
    ->  Int
    ->  [(NodeId, HostAddress, PortNumber)]
    ->  IO ()
connectTo aChan aNum aConnects = do
    aShuffledConnects <- shuffleM aConnects
    forM_ (take aNum aShuffledConnects) $ \(aNodeId, aIp, aPort) -> do
        writeChan aChan $ sendInitDatagram aIp aPort aNodeId

connectToBootNode :: (ManagerMsg msg, ManagerData md) => Chan msg -> md -> IO ()
connectToBootNode aChan ((^.nodeBaseData.bootNodes) -> aBootNodeList) =
    connectTo aChan 1 aBootNodeList

{-
  TODO answerToConnectivityQuery
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
-}
iIsBroadcastNode :: ManagerData md => md -> Bool
iIsBroadcastNode aData = BroadcastNode `elem` aData^.nodeConfig.helloMsg.nodeVariantRoles


answerToClientDisconnected
    ::  ManagerData md
    =>  ManagerMsg msg
    =>  IORef md
    ->  msg
    ->  IO ()
answerToClientDisconnected aMd (toManagerMsg -> ClientIsDisconnected aId aChan) = do
    aData <- readIORef aMd
    whenJust (aId `M.lookup` (aData^.nodes)) $ \aNode -> do
        when (aNode^.status == Noactive) $
            deleteFromFile NetLvl (aData^.myNodeId) aId

        when (aNode^.chan == aChan) $
            modifyIORef aMd (nodes %~ M.delete aId)

answerToClientDisconnected _ _ = pure ()


answerToSendInitDatagram
    :: ManagerData md
    => ManagerMsg msg
    => Chan msg
    -> IORef md
    -> msg
    -> IO ()
answerToSendInitDatagram
    aManagerChan
    aMd
    (toManagerMsg -> SendInitDatagram receiverIp receiverPort aId) = do
        aData <- readIORef aMd
        loging aData $ "answerToSendInitDatagram: " ++
            showHostAddress receiverIp ++ ":" ++ show receiverPort ++ " " ++
            show aId
        unless (aId `M.member` (aData^.nodes)) $ do

            aNodeChan <- newChan
            modifyIORef aMd $ nodes %~ M.insert aId (makeNode aNodeChan)

            void $ forkIO $ do
                aMsg <- makeConnectingRequest
                    (aData^.myNodeId)
                    (aData^.publicPoint)
                    (aData^.privateKey)
                sendPackagedMsg aNodeChan aMsg
                runClient
                    (showHostAddress receiverIp)
                    (fromEnum receiverPort) "/"
                    (socketActor receiverIp aId aManagerChan aNodeChan) `finally`
                        (writeChan aManagerChan $ clientIsDisconnected aId aNodeChan)

answerToSendInitDatagram _ _ _ = pure ()

answerToServerDead
    ::  ManagerMsg a
    =>  Chan a
    ->  PortNumber
    ->  a
    ->  IO ()
answerToServerDead aChan aPort _ =  void $ startServerActor aChan aPort


answerToDisconnectNode
    ::  ManagerData md
    =>  ManagerMsg msg
    =>  md
    ->  msg
    ->  IO ()
answerToDisconnectNode aData (toManagerMsg -> DisconnectNode aId) = do
    loging aData "answerToDisconnectNode"
    whenJust (aId `M.lookup`(aData^.nodes)) $ sendExitMsgToNode

answerToDisconnectNode _ _ = pure ()


answerToInitDatagram
    ::  ManagerData md
    =>  ManagerMsg msg
    =>  IORef md
    ->  msg
    ->  IO ()
answerToInitDatagram aMd
    (toManagerMsg -> InitDatagram aInputChan aHostAdress aDatagram) = do
    modifyIORef aMd $ nodeConfig.helloMsg.nodeVariantRoles %~ lInsert BroadcastNode
    case decode aDatagram of
        Right (aPack @(Unciphered (ConnectingRequest aPublicPoint aId _)))
            | verifyConnectingRequest aPack ->
                answerToInitiatorConnectingMsg (toNodeId aId) aHostAdress aInputChan aPublicPoint aMd
        _                               -> writeChan aInputChan SenderTerminate
answerToInitDatagram _ _                =  pure ()


answerToDatagramMsg
    ::  ManagerData md
    =>  PackageTraceRoutingAction md RequestPackage
    =>  PackageTraceRoutingAction md ResponcePackage
    =>  BroadcastAction md
    =>  ManagerMsg msg
    =>  Chan msg
    ->  IORef md
    ->  p
    ->  msg
    ->  IO ()
answerToDatagramMsg aChan aMd _
    (toManagerMsg -> DatagramMsg aDatagramMsg aId) = do
        whenRight (decode aDatagramMsg) $ \case
            aPack @(Unciphered (ConnectingRequest aPublicPoint aId _))
                | verifyConnectingRequest aPack
                    -> answerToRemoteConnectingMsg (toNodeId aId) aPublicPoint aMd
            Ciphered aCipheredString ->
                answerToPackagedMsg aId aChan aCipheredString aMd
            _                     -> pure ()
answerToDatagramMsg _ _  _ _    =  pure ()

{-
sendPingMsgTo :: (ManagerData md, )
    -- aTimeSpec
    --
-}


class PackageTraceRoutingAction aManagerData aRequest where
    makeAction
        ::  aChan
        ->  IORef aManagerData
        ->  NodeId
        ->  TraceRouting
        ->  aRequest
        ->  IO ()


class BroadcastAction aManagerData where
    makeBroadcastAction
        ::  aChan
        ->  IORef aManagerData
        ->  NodeId
        ->  PackageSignature
        ->  BroadcastThing
        ->  IO ()

answerToPackagedMsg :: (
    ManagerData md,
    PackageTraceRoutingAction md RequestPackage,
    PackageTraceRoutingAction md ResponcePackage,
    BroadcastAction md) =>
    NodeId -> aChan -> CipheredString -> IORef md -> IO ()

answerToPackagedMsg aId aChan aChipredString aMd = do
    aData <- readIORef aMd
    loging aData $ "answerToPackagedMsg: " ++ show aChipredString
    let aDecryptedPacage = do
            key  <- nodeKey =<< (aId `M.lookup`(aData^.nodes))
            decryptChipred key aChipredString
    whenJust aDecryptedPacage $ \case
        PackageTraceRoutingRequest aTraceRouting aRequestPackage ->
            makeAction aChan aMd aId aTraceRouting aRequestPackage
        PackageTraceRoutingResponce aTraceRouting aResponcePackage ->
            makeAction aChan aMd aId aTraceRouting aResponcePackage
        BroadcastRequest aBroadcastSignature aBroadcastThing ->
            makeBroadcastAction aChan aMd aId aBroadcastSignature aBroadcastThing
answerToPackagedMsg _ _ _  _ = return ()


-- aNodeType -> t -> IORef md -> NodeId -> [(NodeId, TimeSpec, Signature)] -> RequestPackage -> IO ()


whenLeft
    ::  Show a
    =>  Show b
    =>  NodeConfigClass aData
    =>  aData
    ->  Either a b
    ->  IO ()
whenLeft aData aMsg@(Left _) = loging aData $ show aMsg
whenLeft _ _ = pure ()


answerToDisconnect :: ManagerData md => [Reason] -> NodeId -> IORef md -> IO ()
answerToDisconnect _ aNodeId aMd = do
    aData <- readIORef aMd
    loging aData "answerToDisconnect"
    whenJust (aNodeId `M.lookup` (aData^.nodes)) sendExitMsgToNode


answerToInitiatorConnectingMsg
    ::  ManagerData md
    =>  NodeId
    ->  HostAddress
    ->  Chan MsgToSender
    ->  PublicPoint
    ->  IORef md
    ->  IO ()
answerToInitiatorConnectingMsg aId aHostAdress aInputChan aPublicPoint aMd = do
    aData <- readIORef aMd
    loging aData $ "answerToInitiatorConnectingMsg from " ++ showHostAddress aHostAdress ++ " " ++ show aId
    if aId `M.member` (aData^.nodes) then do
        loging aData $ "is refused " ++ showHostAddress aHostAdress ++ " " ++ show aId
        writeChan aInputChan SenderTerminate
    else do
        loging aData $ "is accepted " ++ showHostAddress aHostAdress ++ " " ++ show aId
        modifyIORef aMd $ nodes %~ M.insert aId (makeNode aInputChan)
        aNewData <- readIORef aMd
        sendRemoteConnectDatagram aInputChan aNewData
        modifyIORef aMd $ nodes %~ M.adjust (&~ do
            mKey            .= Just (getKay (aNewData^.privateNumber) aPublicPoint)
            status          .= Active
          ) aId

answerToRemoteConnectingMsg
    ::  ManagerData md
    =>  NodeId
    ->  PublicPoint
    ->  IORef md
    ->  IO ()
answerToRemoteConnectingMsg aId aPublicPoint aMd = do
    aData <- readIORef aMd
    loging aData $ "answerToRemoteConnectingMsg from " ++ show aId
    modifyIORef aMd $ nodes %~ M.adjust (&~ do
        mKey            .= Just (getKay (aData^.privateNumber) aPublicPoint)
        status          .= Active
      ) aId


sendRemoteConnectDatagram :: ManagerData md => Chan MsgToSender -> md -> IO ()
sendRemoteConnectDatagram aChan aData = do
    loging aData $ "sendRemoteConnectDatagram"
    sendPackagedMsg aChan =<<  makeConnectingRequest
        (aData^.myNodeId) (aData^.publicPoint) (aData^.privateKey)

{-# DEPRECATED sendDatagramFunc "Use sendPackagedMsg" #-}
sendDatagramFunc :: Chan MsgToSender -> B.ByteString -> IO ()
sendDatagramFunc aChan aMsg = writeChan aChan $ MsgToSender aMsg


sendPackagedMsg :: Chan MsgToSender -> Package -> IO ()
sendPackagedMsg aChan aMsg = sendDatagramFunc aChan $ encode aMsg


makePing
    ::  ManagerMsg a
    =>  Chan a
    ->  HostAddress
    ->  PortNumber
    ->  IO ()
makePing aChan aHostAdress aPortNumber = do
    void $ forkIO $ runClient
        (showHostAddress aHostAdress)
        (fromEnum aPortNumber) "/"
        aSendRecive
   where
     aSendRecive aConnect = void $ race aStoper (aPinger aConnect)

     aStoper = do
         threadDelay $ 2*10^6
         return ()

     aPinger aConnect = do
         aTimeStart <- getTime Realtime
         WS.sendBinaryData aConnect $ encode $ Unciphered PingRequest
         aMsg <- WS.receiveDataMessage aConnect
         let Right (Unciphered (PongResponce aMyHostAdress)) = decode
                $ WS.fromDataMessage aMsg
         aTimeStop <- getTime Realtime
         let aPingTime = diffTimeSpec aTimeStart aTimeStop
         void $ writeChan aChan $ pingRequestInfo
            aHostAdress aPortNumber aPingTime aHostAdress

{-

    race ()

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

-}

{-
type PingAnswer a c = Chan c -> IORef a -> NodeId -> PingPackage -> IO ()
type PongAnswer a c = Chan c -> IORef a -> NodeId -> PongPackage -> IO ()
type InfoPingAnswer a c =
    Chan c -> IORef a -> NodeId -> InfoPingPackage -> IO ()
-}

minusStatusNumber
    ::  NodeBaseDataClass a
    =>  NodeConfigClass a
    =>  IORef a
    ->  NodeId
    ->  IO ()
minusStatusNumber aMd aId = do
    aData <- readIORef aMd
    whenJust (aId `M.lookup` (aData^.nodes)) $ \_ -> do
        loging aData $ "Disconnected: " <> show aId


sendJustPackagedMsg :: Maybe (Chan MsgToSender, Package) -> IO ()
sendJustPackagedMsg x = whenJust x $ uncurry sendPackagedMsg

{-
sendJustDatagram :: Maybe (Chan MsgToSender, PackagedMsg) -> IO ()
sendJustDatagram = sendJustPackagedMsg
-}

makeMsg
    ::  ManagerData s
    =>  NodeId
    ->  s
    ->  (StringKey -> CryptoFailable b)
    ->  Maybe (Chan MsgToSender, b)
makeMsg aId aData func = do
    aNode        <- aId `M.lookup` (aData^.nodes)
    packagedMsg  <- maybeCryptoError . func =<< aNode^.mKey
    pure (nodeChan aNode, packagedMsg)

getStatus :: ManagerData md => NodeId -> md -> Maybe NodeStatus
getStatus aId aMd = (^.status) <$> (aId `M.lookup` (aMd^.nodes))


class GetNodes a where
    getNodes :: ManagerData md => a -> md -> [Node]


instance GetNodes NodeStatus where
    getNodes aStatus aData = filter (\aNode -> aNode^.status == aStatus) $
        aData^.nodes.to M.elems


sendBroadcastThingToNodes
    ::  ManagerData md
    =>  IORef md
    ->  PackageSignature
    ->  BroadcastThing
    ->  IO ()
sendBroadcastThingToNodes aMd aBroadcastSignature aBroadcastThing = do
    aData <- readIORef aMd
    sendToNodes aData aMakeMsg
  where
    aMakeMsg :: StringKey -> CryptoFailable Package
    aMakeMsg = makeCipheredPackage
        (BroadcastRequest aBroadcastSignature aBroadcastThing)


sendToNodes
    ::  ManagerData md
    =>  md
    ->  (StringKey -> CryptoFailable Package)
    ->  IO ()
sendToNodes aData aMakeMsg = forM_ (M.elems $ aData^.nodes) (sendToNode aMakeMsg)

sendToNode :: (StringKey -> CryptoFailable Package) -> Node -> IO ()
sendToNode aMakeMsg aNode = do
    whenJust (aNode^.mKey) $ \aKey -> do
        whenJust (maybeCryptoError $ aMakeMsg aKey) $ \aJustMsg -> do
            sendPackagedMsg (aNode^.chan) aJustMsg


{-
type instance NodeInfoList LogicLvl = [(NodeId, NodePosition)]
type instance NodeInfoList NetLvl   = [(NodeId, HostAddress, PortNumber)]
-}
class FileDB a where
    saveRecordsToNodeListFile   :: MyNodeId -> NodeInfoList a -> IO ()
    readRecordsFromNodeListFile :: MyNodeId -> IO (NodeInfoList a)
    addRecordsToNodeListFile    :: MyNodeId -> NodeInfoList a -> IO ()
    deleteFromFile              :: a -> MyNodeId -> NodeId -> IO ()
    updateFile                  :: MyNodeId -> NodeInfoList a -> IO ()

instance FileDB NetLvl where
    readRecordsFromNodeListFile (MyNodeId aMyNodeId) = do
        aFileContent <- readDataFile $
            "./data/listOfConnects" ++ show aMyNodeId ++ ".txt"
        return $ NodeInfoListNetLvl aFileContent


    saveRecordsToNodeListFile aMyNodeId (NodeInfoListNetLvl aList) =
        writeDataToFile
            ("./data/listOfConnects" ++ show aMyNodeId ++ ".txt")
            aList

    addRecordsToNodeListFile aMyNodeId aRecords = do
        NodeInfoListNetLvl aFileContent <- readRecordsFromNodeListFile aMyNodeId
        let aFilteredRecords = filter
                (\a -> aNotInFile a && aNotIAm a) aFileContent
            -- aNotInLocalHost a = a^._2 /= read "127.0.0.1"
            aNotInFile      a = a `notElem` aFileContent
            aNotIAm         a = toMyNodeId (a^._1) /= aMyNodeId

        addDataToFile
            ("./data/listOfConnects" ++ show aMyNodeId ++ ".txt")
            aFilteredRecords


    deleteFromFile _ aMyNodeId aNodeId = do
        NodeInfoListNetLvl aRecords <-readRecordsFromNodeListFile aMyNodeId
        let aFilteredRecords = filter (\a -> a^._1 /= aNodeId) aRecords
        saveRecordsToNodeListFile aMyNodeId (NodeInfoListNetLvl aFilteredRecords)

    updateFile aMyNodeId (NodeInfoListNetLvl aNewRecords) = do
        NodeInfoListNetLvl aRecords <-readRecordsFromNodeListFile aMyNodeId
        let aIdsForUpdate    = (^._1) <$> aNewRecords
            aFilteredRecords = filter (\a -> a^._1 `notElem` aIdsForUpdate) aRecords
            aUpdatedRecords  = aNewRecords ++ aFilteredRecords

        saveRecordsToNodeListFile aMyNodeId (NodeInfoListNetLvl aUpdatedRecords)


instance FileDB LogicLvl where
    readRecordsFromNodeListFile (MyNodeId aMyNodeId) = do
        aList <- readDataFile $ "./data/listOfPositions" ++ show aMyNodeId ++ ".txt"
        return $ NodeInfoListLogicLvl aList


    saveRecordsToNodeListFile aMyNodeId (NodeInfoListLogicLvl aList) =
        writeDataToFile ("./data/listOfPositions" ++ show aMyNodeId ++ ".txt") aList


    addRecordsToNodeListFile aMyNodeId aRecords = do
        NodeInfoListLogicLvl aFileContent <- readRecordsFromNodeListFile aMyNodeId
        let aFilteredRecords = filter
                (\a -> aNotInFile a && aNotIAm a) aFileContent
            aNotInFile      a = a `notElem` aFileContent
            aNotIAm         a = toMyNodeId (a^._1) /= aMyNodeId

        addDataToFile
            ("./data/listOfPositions" ++ show aMyNodeId ++ ".txt")
            aFilteredRecords

    deleteFromFile _ aMyNodeId aNodeId = do
        NodeInfoListLogicLvl aRecords <- readRecordsFromNodeListFile aMyNodeId
        let aFilteredRecords = filter (\a -> a^._1 /= aNodeId) aRecords
        saveRecordsToNodeListFile
            aMyNodeId (NodeInfoListLogicLvl aFilteredRecords)

    updateFile aMyNodeId (NodeInfoListLogicLvl aNewRecords) = do
        NodeInfoListLogicLvl aRecords <-readRecordsFromNodeListFile aMyNodeId
        let aIdsForUpdate    = (^._1) <$> aNewRecords
            aFilteredRecords = filter (\a -> a^._1 `notElem` aIdsForUpdate) aRecords
            aUpdatedRecords  = aNewRecords ++ aFilteredRecords

        saveRecordsToNodeListFile aMyNodeId (NodeInfoListLogicLvl aUpdatedRecords)



lInsert :: NodeVariantRole -> [NodeVariantRole] -> [NodeVariantRole]
lInsert _ [BootNode] = [BootNode]
lInsert aElem aList  = S.toList . S.fromList $ aElem : aList





--------------------------------------------------------------------------------
