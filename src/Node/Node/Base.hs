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

module Node.Node.Base (
            BroadcastAction(..)
        ,   PackageTraceRoutingAction(..)
        ,   sendBroadcastThingToNodes
        ,   answerToSendInitDatagram
        ,   answerToDatagramMsg
        ,   answerToInitDatagram
        ,   baseNodeOpts
        ,   initShading
        ,   sendExitMsgToNode
  ) where

import              System.Random
import              System.Random.Shuffle
import              Control.Monad.State.Lazy
import              Control.Monad.Extra
import              Control.Exception
import              Control.Concurrent
import              Crypto.Error
import              Crypto.PubKey.ECC.ECDSA
import qualified    Data.Map                        as M
import qualified    Data.Set                        as S
import              Data.IORef
import              Data.Serialize
import              Lens.Micro.Mtl
import              Lens.Micro
import              Data.Hex

import              Node.FileDB.FileServer
import              Service.Network.WebSockets.Client
import              Service.Network.Base
import              Service.Monad.Option
import              Sharding.Space.Point
import              Sharding.Space.Shift
import              Sharding.Sharding
import              Node.Node.Types
import              Node.Crypto
import              Node.Data.Key
import              Node.Data.NetPackage
import              Node.Node.Base.Server
import              Node.Data.MakeAndSendTraceRouting
import              Node.Data.GlobalLoging
import              Service.InfoMsg
import              Data.Maybe

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
    opt isQueryPositions        $ answerToQueryPositions aMd
    opt isDisconnectNode        $ answerToDisconnectNode  aData


pattern Chan :: Chan MsgToSender -> Node
pattern Chan aChan <- ((^.chan) -> aChan)


sendExitMsgToNode :: Node -> IO ()
sendExitMsgToNode (Chan aChan) = do
    sendPackagedMsg aChan disconnectRequest
    writeChan       aChan SenderTerminate
sendExitMsgToNode _ = error "Node.Node.Base.sendExitMsgToNode"

pattern Head :: forall a b. a -> b -> [(a, b)]
pattern Head aId aElem <- (aId, aElem):_

pattern PositionOfFirst :: forall a. NodePosition -> [(a, Node)]
pattern PositionOfFirst aPosition <- Head _ ((^.nodePosition) -> Just aPosition)

preferedBroadcastCount :: Int
preferedBroadcastCount = 4

answerToQueryPositions :: ManagerData md =>  ManagerMsg msg => IORef md -> msg -> IO()
answerToQueryPositions aMd _ = do
    aData <- readIORef aMd
    let ids = [aId | (aId, aNode) <- M.toList $ aData^.nodes, isNothing $ aNode^.nodePosition]
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Node posiotion request: " ++ show ids
    makeAndSendTo aData ids NodePositionRequestPackage



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

    aPosChan <- newChan
    aConChan <- newChan
    writeChan (aData^.fileServerChan) $
         FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aConChan
    writeChan (aData^.fileServerChan) $
         FileActorRequestLogicLvl $ ReadRecordsFromNodeListFile aPosChan

    NodeInfoListNetLvl   aConnectList   <- readChan aConChan
    NodeInfoListLogicLvl aPossitionList <- readChan aPosChan

    let aWait = aBroadcastNum >= preferedBroadcastCount{- || aBroadcastNum <= 6 -} || aUnActiveNum /= 0
        aFilteredPositions = S.fromList . M.elems $ M.intersection
            (M.fromList aPossitionList) (M.fromList aConnectList)

    if  | aWait             -> return ()
        | null aConnectList -> connectToBootNode aChan aData
        | iDontHaveAPosition aData -> if
            | aBroadcastNum == 0 -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                    "Init. Connect to first broadcast node."
                connectTo aChan 1 aConnectList
            | PositionOfFirst (NodePosition (Point x y)) <- aBroadcasts -> do
                aDeltaX <- randomRIO (0, 2000)
                aDeltaY <- randomRIO (0, 2000)
                initShading aChan aMd $ MyNodePosition $ Point
                    (x + aDeltaX - 1000) (y + aDeltaY - 1000)

            | Head aNodeId _ <- aBroadcasts -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Request of a node position of the " ++ show aNodeId ++ "."
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
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                "Request of the " ++ show aConnectsNum ++ " connects."
            connectTo aChan aConnectsNum aPreferedConnects
        |   otherwise -> error $ "!!!!!!!!!!!" ++ show aBroadcastNum ++ " " ++ show aUnActiveNum
      -- if we don't find anybody send message error
      -- TODO: optimize by net and logic lvl
      --  | aBroadcastNum > 6     -> undefined


initShading aChan aMd aPoint = do
    aData <- readIORef aMd
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
        "Init. Take new logic coordinates " ++ show aPoint ++ "."

    aChanOfSharding <- newChan
    makeShardingNode (aData^.myNodeId) aChanOfSharding
        aChan aPoint (aData^.infoMsgChan)
    modifyIORef aMd (&~ do
        myNodePosition .= Just aPoint
        shardingChan   .= Just aChanOfSharding)

iDontHaveAPosition :: ManagerData md => md -> Bool
iDontHaveAPosition aData = isNothing $ aData^.myNodePosition


connectTo
    ::  ManagerMsg msg
    =>  Chan msg
    ->  Int
    ->  [(NodeId, Connect)]
    ->  IO ()
connectTo aChan aNum aConnects = do
    aShuffledConnects <- shuffleM aConnects
    forM_ (take aNum aShuffledConnects) $ \(aNodeId, Connect aIp aPort) ->
        writeChan aChan $ sendInitDatagram aIp aPort aNodeId

connectToBootNode :: (ManagerMsg msg, ManagerData md) => Chan msg -> md -> IO ()
connectToBootNode aChan aData = do
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info "Try connect to a bootNode."
    let aBootNodeList = aData^.nodeBaseData.bootNodes
    when (null aBootNodeList) $ do
        let aError = "aBootNodeList is empty!!! Check config."
        writeLog (aData^.infoMsgChan) [NetLvlTag] Error aError
        error aError
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
        "Try connect to the bootNode " ++ show (head aBootNodeList) ++ "."
    connectTo aChan 1 aBootNodeList


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
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
            "Request of connect to " ++
            showHostAddress receiverIp ++ ":" ++ show receiverPort ++ " " ++
            show aId ++ "."
        unless (aId `M.member` (aData^.nodes)) $ do
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                "Try connect to " ++ show aId ++ "."
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
                        writeChan aManagerChan (clientIsDisconnected aId aNodeChan)

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
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info
        "answerToDisconnectNode"
    whenJust (aData^.nodes.at aId) sendExitMsgToNode

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
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info
            "I am a broadcast node."
        modifyIORef aMd $ iAmBroadcast .~ True
    case decode aDatagram of
        Right (aPack @(Unciphered (ConnectingRequest aPublicPoint aId aPortNumber _)))
            | verifyConnectingRequest aPack -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Request of connect from " ++ show aId ++ "."
                answerToInitiatorConnectingMsg
                    (toNodeId aId)
                    aHostAdress
                    aInputChan
                    aPublicPoint
                    aPortNumber
                    aMd
        _ -> do
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                "Request of connect is bad."
            writeChan aInputChan SenderTerminate
answerToInitDatagram _ _                =  pure ()


answerToDatagramMsg
    ::  ManagerData md
    =>  PackageTraceRoutingAction md RequestPackage
    =>  PackageTraceRoutingAction md ResponsePackage
    =>  BroadcastAction md
    =>  ManagerMsg msg
    =>  Chan msg
    ->  IORef md
    ->  p
    ->  msg
    ->  IO ()
answerToDatagramMsg aChan aMd _
    (toManagerMsg -> DatagramMsg aDatagramMsg aId) =
        whenRight (decode aDatagramMsg) $ \case
            aPack @(Unciphered (ConnectingRequest aPublicPoint _ _ _))
                | verifyConnectingRequest aPack
                    -> answerToRemoteConnectingMsg aId aPublicPoint aMd
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
    =>  PackageTraceRoutingAction md ResponsePackage
    =>  BroadcastAction md
    =>  ManagerMsg msg
    =>  NodeId
    ->  Chan msg
    ->  CipheredString
    ->  IORef md
    ->  IO ()

answerToPackagedMsg aId aChan aCipheredString@(CipheredString aStr) aMd = do
    aData <- readIORef aMd
    let aDecryptedPackage = do
            key  <- _mKey =<< aData^.nodes.at aId
            decryptChipred key aCipheredString
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
        "Received a message " ++ show aDecryptedPackage ++ " from " ++ show aId ++ "."

    whenJust aDecryptedPackage $ \case
        PackageTraceRoutingRequest aTraceRouting aRequestPackage ->
            makeAction aChan aMd aId aTraceRouting aRequestPackage
        PackageTraceRoutingResponse aTraceRouting aResponsePackage ->
            makeAction aChan aMd aId aTraceRouting aResponsePackage
        BroadcastRequest aBroadcastSignature aBroadcastThing ->
            makeBroadcastAction aChan aMd aId aBroadcastSignature aBroadcastThing


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
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
        "Make answer to initiator connecting msg from " ++ showHostAddress aHostAdress ++ " " ++ show aId
    if aId `M.member` (aData^.nodes) then do
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
            "Is refused " ++ showHostAddress aHostAdress ++ " " ++ show aId
        writeChan aInputChan SenderTerminate
    else do
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
            "Is accepted " ++ showHostAddress aHostAdress ++ " " ++ show aId
        let aKey = getStringKey (aData^.privateNumber) aPublicPoint
            aNode = makeNode aInputChan aHostAdress aPortNumber &~ do
                mKey            .= Just aKey
                status          .= Active

        modifyIORef aMd $ nodes %~ M.insert aId aNode
        aNewData <- readIORef aMd
        sendRemoteConnectDatagram aInputChan aNewData
        makeAndSendTo aNewData [aId] BroadcastListRequest
        makeAndSendTo aNewData [aId] IsYouBrodcast


answerToRemoteConnectingMsg
    ::  ManagerData md
    =>  NodeId
    ->  PublicPoint
    ->  IORef md
    ->  IO ()
answerToRemoteConnectingMsg aId aPublicPoint aMd = do
    aData <- readIORef aMd
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
        "The node " ++ show aId ++ " confirmed the connection."
    modifyIORef aMd $ nodes %~ M.adjust (&~ do
        mKey            .= Just (getStringKey (aData^.privateNumber) aPublicPoint)
        status          .= Active
        isBroadcast     .= True
      ) aId
    aNewData <- readIORef aMd
    makeAndSendTo aNewData [aId] BroadcastListRequest


sendRemoteConnectDatagram :: ManagerData md => Chan MsgToSender -> md -> IO ()
sendRemoteConnectDatagram aChan aData = do
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info "Send of connection confirmetion."
    sendPackagedMsg aChan =<<  makeConnectingRequest
        (aData^.myNodeId)
        (aData^.publicPoint)
        (aData^.nodeBaseData.outPort)
        (aData^.privateKey)



sendBroadcastThingToNodes
    ::  ManagerData md
    =>  IORef md
    ->  PackageSignature
    ->  BroadcastThing
    ->  IO ()
sendBroadcastThingToNodes aMd aBroadcastSignature aBroadcastThing = do
    aData <- readIORef aMd
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
        "Broadcasting to neighbors a " ++ show aBroadcastThing ++ "."
    forM_ (M.elems $ aData^.nodes) (sendToNode aMakeMsg)
  where
    aMakeMsg :: StringKey -> CryptoFailable Package
    aMakeMsg = makeCipheredPackage
        (BroadcastRequest aBroadcastSignature aBroadcastThing)

--------------------------------------------------------------------------------

whenRight :: Show a => Either a t -> (t -> IO ()) -> IO ()
whenRight aElem aFunc = case aElem of
    Left  aError    -> putStrLn $ "Error: " ++ show aError
    Right aJustElem -> aFunc aJustElem
