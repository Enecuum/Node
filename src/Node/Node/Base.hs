{-# LANGUAGE
        LambdaCase
    ,   ViewPatterns
    ,   MultiWayIf
    ,   ScopedTypeVariables
    ,   MultiParamTypeClasses
    ,   FlexibleContexts
    ,   PatternSynonyms
    ,   FlexibleInstances
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
import              Data.Hex

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
import              Node.Node.Base.Server
import              Node.Data.MakeTraceRouting
import              Node.Data.MakeAndSendTraceRouting
import              Node.Data.GlobalLoging

baseNodeOpts
    ::  ManagerData md2
    =>  ManagerData md1
    =>  ManagerMsg msg
    =>  Chan msg
    ->  IORef md1
    ->  md2
    ->  Options msg ()
baseNodeOpts aChan aMd aData = do
    opt isSendInitDatagram      $ answerToSendInitDatagram aChan aMd
    opt isServerIsDead          $ answerToServerDead aChan defaultServerPort
    opt isConnectivityQuery     $ answerToConnectivityQuery aChan aMd
    opt isDisconnectNode        $ answerToDisconnectNode    aData


pattern Chan :: Chan MsgToSender -> Node
pattern Chan aChan <- ((^.chan) -> aChan)


sendExitMsgToNode :: Node -> IO ()
sendExitMsgToNode (Chan aChan) = do
    sendPackagedMsg aChan disconnectRequest
    writeChan       aChan SenderTerminate


pattern Head aId aElem <- (aId, aElem):_
pattern PositionOfFirst aPosition <- Head _ ((^.nodePosition) -> Just aPosition)

preferedBroadcastCount :: Int
preferedBroadcastCount = 4

-- TODO optimization by ping
-- TODO optimization by routing
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
                loging aData $ "Init. Take new logic coordinates " ++ show aMyNodePosition ++ "."
                loging aData $ "Init. A sharding lvl init."
                aChanOfSharding <- newChan
                makeShardingNode aMyNodeId aChanOfSharding aChan aMyNodePosition
                modifyIORef aMd (&~ do
                    myNodePosition .= Just aMyNodePosition
                    shardingChan   .= Just aChanOfSharding)
            | Head aNodeId _ <- aBroadcasts -> do
                loging aData $ "Request of a node position of the " ++ show aNodeId ++ "."
                makeAndSendTo aData [aNodeId] NodePositionRequestPackage

        |   aBroadcastNum < preferedBroadcastCount,
            Just aMyNodePosition <- aData^.myNodePosition -> do

            let aPositionOfPreferedConnect = findNearestNeighborPositions
                    aMyNodePosition aFilteredPositions
                isPreferedByPositon a = (a^._2) `elem`aPositionOfPreferedConnect
                aNodesId = (^._1) <$> filter isPreferedByPositon aPossitionList
                isPreferedById a = (a^._1)  `elem` aNodesId
                aPreferedConnects = filter isPreferedById aConnectList
                aConnectsNum = preferedBroadcastCount - aBroadcastNum
            loging aData $ "Request of the " ++ show aConnectsNum ++ " connects."
            connectTo aChan aConnectsNum aPreferedConnects

      -- if we don't find anybody send message error
      -- TODO: optimize by net and logic lvl
      --  | aBroadcastNum > 6     -> undefined


iDontHaveAPosition :: ManagerData md => md -> Bool
iDontHaveAPosition aData = aData^.myNodePosition /= Nothing


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
connectToBootNode aChan aData = do
    loging aData "Try connect to a bootNode."
    let aBootNodeList = aData^.nodeBaseData.bootNodes
    when (null aBootNodeList) $ do
        let aError = "aBootNodeList is empty!!! Check config."
        loging aData aError
        error aError
    loging aData $ "Try connect to the bootNode " ++ show (head aBootNodeList) ++ "."
    connectTo aChan 1 aBootNodeList


answerToClientDisconnected
    ::  ManagerData md
    =>  ManagerMsg msg
    =>  IORef md
    ->  msg
    ->  IO ()
answerToClientDisconnected aMd (toManagerMsg -> ClientIsDisconnected aId aChan) = do
    aData <- readIORef aMd
    whenJust (aData^.nodes.at aId) $ \aNode -> do
        when (aNode^.status == Noactive) $ do
            deleteFromFile NetLvl (aData^.myNodeId) aId
            when (aId `elem` ((^._1) <$> aData^.nodeBaseData.bootNodes)) $
                loging aData $ "The " ++ show aId ++ " bootNode is unreachable."

        when (aNode^.chan == aChan) $ do
            loging aData $ "The " ++ show aId ++ " is disconnected."
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
        loging aData $ "Request of connect to " ++
            showHostAddress receiverIp ++ ":" ++ show receiverPort ++ " " ++
            show aId ++ "."
        unless (aId `M.member` (aData^.nodes)) $ do
            loging aData $ "Try connect to " ++ show aId ++ "."
            aNodeChan <- newChan
            modifyIORef aMd $ nodes %~ M.insert aId
                (makeNode aNodeChan receiverIp receiverPort)

            void $ forkIO $ do
                aMsg <- makeConnectingRequest
                    (aData^.myNodeId)
                    (aData^.publicPoint)
                    (aData^.nodeBaseData.outPort)
                    (aData^.privateKey)
                sendPackagedMsg aNodeChan aMsg
                runClient
                    (showHostAddress receiverIp)
                    (fromEnum receiverPort) "/"
                    (socketActor receiverIp aId aManagerChan aNodeChan) `finally`
                        (writeChan aManagerChan $ clientIsDisconnected aId aNodeChan)

answerToSendInitDatagram _ _ _ = pure ()


answerToServerDead :: ManagerMsg a => Chan a -> PortNumber -> a -> IO ()
answerToServerDead aChan aPort _ = void $ startServerActor aChan aPort


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
    aData <- readIORef aMd
    unless (aData^.iAmBroadcast) $ do
        loging aData "I am a broadcast node."
        modifyIORef aMd $ iAmBroadcast .~ True
    case decode aDatagram of
        Right (aPack @(Unciphered (ConnectingRequest aPublicPoint aId aPortNumber _)))
            | verifyConnectingRequest aPack -> do
                loging aData $ "Request of connect from " ++ show aId ++ "."
                answerToInitiatorConnectingMsg
                    (toNodeId aId)
                    aHostAdress
                    aInputChan
                    aPublicPoint
                    aPortNumber
                    aMd
        _ -> do
            loging aData $ "Request of connect is bad."
            writeChan aInputChan SenderTerminate
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
        aData <- readIORef aMd
        whenRight (decode aDatagramMsg) $ \case
            aPack @(Unciphered (ConnectingRequest aPublicPoint aId _ _))
                | verifyConnectingRequest aPack
                    -> answerToRemoteConnectingMsg (toNodeId aId) aPublicPoint aMd
            Ciphered aCipheredString ->
                answerToPackagedMsg aId aChan aCipheredString aMd
            _                     -> pure ()
answerToDatagramMsg _ _  _ _    =  pure ()


class PackageTraceRoutingAction aManagerData aRequest where
    makeAction
        ::  ManagerMsg msg
        =>  Chan msg
        ->  IORef aManagerData
        ->  NodeId
        ->  TraceRouting
        ->  aRequest
        ->  IO ()


class BroadcastAction aManagerData where
    makeBroadcastAction
        ::  ManagerMsg msg
        =>  Chan msg
        ->  IORef aManagerData
        ->  NodeId
        ->  PackageSignature
        ->  BroadcastThing
        ->  IO ()

answerToPackagedMsg
    ::  ManagerData md
    =>  PackageTraceRoutingAction md RequestPackage
    =>  PackageTraceRoutingAction md ResponcePackage
    =>  BroadcastAction md
    =>  ManagerMsg msg
    =>  NodeId
    ->  Chan msg
    ->  CipheredString
    ->  IORef md
    ->  IO ()

answerToPackagedMsg aId aChan aCipheredString@(CipheredString aStr) aMd = do
    aData <- readIORef aMd
    loging aData $ "Received a message " ++ show (hex aStr) ++ " from " ++ show aId ++ "."
    let aDecryptedPacage = do
            key  <- _mKey =<< aData^.nodes.at aId
            decryptChipred key aCipheredString
    whenJust aDecryptedPacage $ \case
        PackageTraceRoutingRequest aTraceRouting aRequestPackage ->
            makeAction aChan aMd aId aTraceRouting aRequestPackage
        PackageTraceRoutingResponce aTraceRouting aResponcePackage ->
            makeAction aChan aMd aId aTraceRouting aResponcePackage
        BroadcastRequest aBroadcastSignature aBroadcastThing ->
            makeBroadcastAction aChan aMd aId aBroadcastSignature aBroadcastThing
answerToPackagedMsg _ _ _  _ = return ()


answerToDisconnect :: ManagerData md => [Reason] -> NodeId -> IORef md -> IO ()
answerToDisconnect _ aNodeId aMd = do
    aData <- readIORef aMd
    whenJust (aData^.nodes.at aNodeId) $ \aNode -> do
        loging aData $ "Make answer to disconnect of " ++ show aNodeId ++ "."
        sendExitMsgToNode aNode


answerToInitiatorConnectingMsg
    ::  ManagerData md
    =>  NodeId
    ->  HostAddress
    ->  Chan MsgToSender
    ->  PublicPoint
    ->  PortNumber
    ->  IORef md
    ->  IO ()
answerToInitiatorConnectingMsg aId aHostAdress aInputChan aPublicPoint aPortNumber aMd = do
    aData <- readIORef aMd
    loging aData $ "Make answer to initiator connecting msg from " ++ showHostAddress aHostAdress ++ " " ++ show aId
    if aId `M.member` (aData^.nodes) then do
        loging aData $ "Is refused " ++ showHostAddress aHostAdress ++ " " ++ show aId
        writeChan aInputChan SenderTerminate
    else do
        loging aData $ "Is accepted " ++ showHostAddress aHostAdress ++ " " ++ show aId
        let aKey = getKey (aData^.privateNumber) aPublicPoint
            aNode = (makeNode aInputChan aHostAdress aPortNumber) &~ do
                mKey            .= Just aKey
                status          .= Active

        modifyIORef aMd $ nodes %~ M.insert aId aNode
        aNewData <- readIORef aMd
        sendRemoteConnectDatagram aInputChan aNewData
        makeAndSendTo aNewData [aId] BroadcastListRequest


answerToRemoteConnectingMsg
    ::  ManagerData md
    =>  NodeId
    ->  PublicPoint
    ->  IORef md
    ->  IO ()
answerToRemoteConnectingMsg aId aPublicPoint aMd = do
    aData <- readIORef aMd
    loging aData $ "The node " ++ show aId ++ " confirmed the connection."
    modifyIORef aMd $ nodes %~ M.adjust (&~ do
        mKey            .= Just (getKey (aData^.privateNumber) aPublicPoint)
        status          .= Active
      ) aId
    aNewData <- readIORef aMd
    makeAndSendTo aNewData [aId] BroadcastListRequest


sendRemoteConnectDatagram :: ManagerData md => Chan MsgToSender -> md -> IO ()
sendRemoteConnectDatagram aChan aData = do
    loging aData $ "Send of connection confirmetion."
    sendPackagedMsg aChan =<<  makeConnectingRequest
        (aData^.myNodeId)
        (aData^.publicPoint)
        (aData^.nodeBaseData.outPort)
        (aData^.privateKey)


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


sendBroadcastThingToNodes
    ::  ManagerData md
    =>  IORef md
    ->  PackageSignature
    ->  BroadcastThing
    ->  IO ()
sendBroadcastThingToNodes aMd aBroadcastSignature aBroadcastThing = do
    aData <- readIORef aMd
    loging aData $ "Broadcasting to neighbors a " ++ show aBroadcastThing ++ "."
    forM_ (M.elems $ aData^.nodes) (sendToNode aMakeMsg)
  where
    aMakeMsg :: StringKey -> CryptoFailable Package
    aMakeMsg = makeCipheredPackage
        (BroadcastRequest aBroadcastSignature aBroadcastThing)


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
--------------------------------------------------------------------------------
