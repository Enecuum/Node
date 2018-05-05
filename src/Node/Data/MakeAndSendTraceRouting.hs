{-# LANGUAGE
        ViewPatterns
    ,   MultiWayIf
    ,   LambdaCase
    ,   ScopedTypeVariables
    ,   MultiParamTypeClasses
    ,   FlexibleContexts
    ,   PatternSynonyms
    ,   FlexibleInstances
    ,   UndecidableInstances
  #-}

module Node.Data.MakeAndSendTraceRouting where

import              Control.Monad.State.Lazy
import              Control.Monad.Extra
import              Control.Concurrent
import              Crypto.Error
import              Data.List.Extra
import qualified    Data.Map                        as M
import              Data.Serialize
import              Lens.Micro
import              Lens.Micro.GHC()


import              Sharding.Space.Point
import              Node.Node.Types
import              Node.Crypto
import              Node.Data.Data
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.MakeTraceRouting
import              Sharding.Space.Distance


class MakeAndSendTraceRouting aReques address where
    makeAndSendTo :: ManagerData md => md -> address -> aReques -> IO ()


instance (LevelRequestContractor aLvl, Serialize (Request aLvl)) =>
    MakeAndSendTraceRouting (Request aLvl) [NodeId] where
        makeAndSendTo aData aNodeIds aRequest = do
            aPackageSignature <- makePackageSignature aData aRequest
            let aRequestPackage = request aRequest aPackageSignature
            forM_ aNodeIds $ \aNodeId -> whenJust (aData^.nodes.at aNodeId)
                $ \aNode -> do
                    aTraceRouting <- makeTraceRouting
                        aData aRequestPackage (ToNode aNodeId)
                    let aNewRequest = PackageTraceRoutingRequest
                            aTraceRouting aRequestPackage
                    sendToNode (makeCipheredPackage aNewRequest) aNode

instance (LevelRequestContractor aLvl, Serialize (Request aLvl)) =>
    MakeAndSendTraceRouting (Request aLvl) NodePosition where
        makeAndSendTo aData (toNodePosition -> aPointTo) aRequest =
            whenJust (aData^.myNodePosition) $ \aMyPosition -> do
                aPackageSignature <- makePackageSignature aData aRequest
                let aRequestPackage = request aRequest aPackageSignature
                    aPointFrom = toNodePosition aMyPosition :: PointFrom

                aTraceRouting <- makeTraceRouting
                    aData
                    (aRequestPackage, aPointTo, aPointFrom)
                    (ToDirect aPointFrom aPointTo)
                let aNewRequest = PackageTraceRoutingRequest
                        aTraceRouting aRequestPackage
                whenJust (getClosedNodeByDirectUnsafe aData (toNodePosition aPointTo)) $
                    sendToNode (makeCipheredPackage aNewRequest)

class SendResponseTo aLvl where
    sendResponseTo
        :: ManagerData md
        =>  TraceRouting
        ->  md
        ->  Request aLvl
        ->  PackageSignature
        ->  Response aLvl
        ->  IO ()
--

instance (LevelRequestContractor aLvl, Serialize (Response aLvl)) => SendResponseTo aLvl where
    sendResponseTo aTraceRouting aData aRequest aSignature aNetPackage = do
        let (aNode, aTrace) = getClosedNode aTraceRouting aData
            aRequestPackage = request aRequest aSignature

        aResponsePackageSignature <- makePackageSignature aData
            (aNetPackage, aRequestPackage)
        sendResponse aNode
            (makeNewTraceRouting aTrace aTraceRouting)
            (response aRequestPackage aNetPackage aResponsePackageSignature)


class LevelRequestContractor aLvl where
    request :: Request aLvl -> PackageSignature -> RequestPackage
    response :: RequestPackage -> Response aLvl -> PackageSignature -> ResponsePackage

instance LevelRequestContractor NetLvl where
    request  = RequestNetLvlPackage
    response = ResponseNetLvlPackage

instance LevelRequestContractor LogicLvl where
    request = RequestLogicLvlPackage
    response = ResponseLogicLvlPackage

instance LevelRequestContractor MiningLvl where
    request = RequestMiningLvlPackage
    response = ResponseMiningLvlPackage

--instance MakeAndSendTraceRouting (Response NetLvl) where


sendToNode :: (StringKey -> CryptoFailable Package) -> Node -> IO ()
sendToNode aMakeMsg aNode = whenJust (aNode^.mKey) $ \aKey ->
    whenJust (maybeCryptoError $ aMakeMsg aKey) $ \aJustMsg ->
        sendPackagedMsg (aNode^.chan) aJustMsg


sendPackagedMsg :: Chan MsgToSender -> Package -> IO ()
sendPackagedMsg aChan aMsg = writeChan aChan $ MsgToSender $ encode aMsg


closedToPointNeighbor
    ::  NodeBaseDataClass s
    =>  DistanceTo Node b
    =>  s
    ->  b
    ->  Bool
    ->  [Node]
closedToPointNeighbor aData aPointTo aFilterBool =
    sortOn (`distanceTo` aPointTo) $ aFilter $ M.elems $ aData^.nodes
  where
    aFilter = if aFilterBool then filter (^.isBroadcast) else id


getClosedNodeByDirect :: ManagerData md => md -> Point -> Bool -> Maybe Node
getClosedNodeByDirect aData aPoint aFilterBool =
    case closedToPointNeighbor aData aPoint aFilterBool of
        aNode:_ | not $ amIClose aData aNode (fromPoint aPoint :: PointTo)
                -> Just aNode
        _       -> Nothing


getClosedNodeByDirectUnsafe ::  ManagerData md => md -> Point -> Maybe Node
getClosedNodeByDirectUnsafe aData aPoint =
    case closedToPointNeighbor aData aPoint False of
        aNode:_ -> Just aNode
        _       -> Nothing

-- 1 ------------------------------------------------ 4
--                                                    |
--       2 -------------------------------------------+

amIClose
    ::  DistanceTo MyNodePosition aPointB
    =>  DistanceTo aPointA aPointB
    =>  ManagerData md
    =>  md
    ->  aPointA
    ->  aPointB
    ->  Bool
amIClose aData aNode aPointTo = if
    | Just aPosition <- aData^.myNodePosition,
        distanceTo aPosition aPointTo < distanceTo aNode aPointTo -> True
    | otherwise -> False


--
getClosedNode
    ::  ManagerData md
    =>  TraceRouting
    ->  md
    ->  (Maybe Node, [PackageSignature])
getClosedNode aTraceRouting aData = case aTraceRouting of
    ToDirect _ aPointTo aTrace
        | Just aNextNodeId <- lookupNextNode aTrace -> do
            let aNewTrace = traceDrop aNextNodeId aTrace
            (aNextNodeId `M.lookup` (aData^.nodes), aNewTrace)
        | otherwise -> (getClosedNodeByDirect aData (toPoint aPointTo) False, cleanTrace aTrace)
    ToNode _ (PackageSignature (toNodeId -> aNodeId) _ _) ->
        (aNodeId `M.lookup` (aData^.nodes), [])

  where
    cleanTrace aTrace = filter aPredicat $ dropWhile aPredicat aTrace
      where
        aPredicat (PackageSignature aNodeId _ _) = aData^.myNodeId /= aNodeId

    lookupNextNode aTrace = if
        | x:_ <- aIntersect -> Just x
        | otherwise         -> Nothing
      where
        aIntersect      = (signatureToNodeId <$> aTrace) `intersect` aNeighborList
        aNeighborList   = M.keys (aData^.nodes)

--
sendResponse :: Maybe Node -> TraceRouting -> ResponsePackage -> IO ()
sendResponse aNode aTraceRouting aPackageResponse = whenJust aNode $
    sendToNode (makeResponse aTraceRouting aPackageResponse)


makeNewTraceRouting :: [PackageSignature] -> TraceRouting -> TraceRouting
makeNewTraceRouting aSignatures = \case
    ToDirect aPointFrom aPointTo _  -> ToDirect aPointFrom aPointTo aSignatures
    ToNode aNodeId (PackageSignature (toNodeId -> aId) aTime aSig) ->
        ToNode aId (PackageSignature (toMyNodeId aNodeId) aTime aSig)


traceDrop :: NodeId -> [PackageSignature] -> [PackageSignature]
traceDrop aNextNodeId = dropWhile
    (\(PackageSignature (toNodeId -> aId) _ _) -> aId /= aNextNodeId)


makeResponse
    ::  TraceRouting
    ->  ResponsePackage
    ->  StringKey
    ->  CryptoFailable Package
makeResponse aTraceRouting aResponse = makeCipheredPackage
    (PackageTraceRoutingResponse aTraceRouting aResponse)


signatureToNodeId :: PackageSignature -> NodeId
signatureToNodeId (PackageSignature (toNodeId -> aNodeId) _ _) = aNodeId

--------------------------------------------------------------------------------
