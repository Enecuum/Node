{-# LANGUAGE
        LambdaCase
    ,   ViewPatterns
    ,   MultiWayIf
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
        makeAndSendTo aData (toNodePosition -> aPointTo) aRequest = do
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


class LevelRequestContractor aLvl where
    request :: Request aLvl -> PackageSignature -> RequestPackage

instance LevelRequestContractor NetLvl where
    request = RequestNetLvlPackage

instance LevelRequestContractor LogicLvl where
    request = RequestLogicLvlPackage

--instance MakeAndSendTraceRouting (Responce NetLvl) where


sendToNode :: (StringKey -> CryptoFailable Package) -> Node -> IO ()
sendToNode aMakeMsg aNode = do
    whenJust (aNode^.mKey) $ \aKey -> do
        whenJust (maybeCryptoError $ aMakeMsg aKey) $ \aJustMsg -> do
            sendPackagedMsg (aNode^.chan) aJustMsg


sendPackagedMsg :: Chan MsgToSender -> Package -> IO ()
sendPackagedMsg aChan aMsg = writeChan aChan $ MsgToSender $ encode aMsg


closedToPointNeighbor
    ::  NodeBaseDataClass s
    =>  DistanceTo Node b
    =>  s
    ->  b
    ->  [Node]
closedToPointNeighbor aData aPointTo = sortOn
    (\n -> distanceTo n aPointTo) $ M.elems $ aData^.nodes


getClosedNodeByDirect :: ManagerData md => md -> Point -> Maybe Node
getClosedNodeByDirect aData aPoint =
    case closedToPointNeighbor aData aPoint of
        aNode:_ | not $ amIClose aData aNode (fromPoint aPoint :: PointTo)
                -> Just aNode
        _       -> Nothing

getClosedNodeByDirectUnsafe ::  ManagerData md => md -> Point -> Maybe Node
getClosedNodeByDirectUnsafe aData aPoint =
    case closedToPointNeighbor aData aPoint of
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




--------------------------------------------------------------------------------
