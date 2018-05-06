{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Node.Data.Verification (Verification(..)) where

import Data.Serialize

import Node.Data.NetPackage
import Node.Crypto
import Node.Data.Key

class Verification a where
  verify :: a -> Bool


instance Verification ResponsePackage where
    verify = \case
        ResponseNetLvlPackage aRequestPackage aResponse aPackageSignature ->
            verify aRequestPackage &&
            verify (aPackageSignature, (aResponse, aRequestPackage))
        ResponseLogicLvlPackage aRequestPackage aResponse aPackageSignature ->
            verify aRequestPackage &&
            verify (aPackageSignature, (aResponse, aRequestPackage))


instance Verification RequestPackage where
    verify = \case
        RequestLogicLvlPackage aPackage aPackageSignature ->
            verify (aPackageSignature, aPackage)
        RequestNetLvlPackage   aPackage aPackageSignature ->
            verify (aPackageSignature, aPackage)
        RequestMiningLvlPackage aPackage aPackageSignature ->
            verify (aPackageSignature, aPackage)


instance Verification (TraceRouting, RequestPackage) where
    verify = \case
        (ToNode _ aPackageSignature, aMsg)                ->
            verify (aPackageSignature, aMsg)

        (ToDirect aPointFrom aPointTo [aSignature], aMsg) ->
            verify aMsg &&
            verify (aSignature, (aMsg, aPointTo, aPointFrom))

        (ToDirect aPointFrom aPointTo (aS:xS), aMsg)      ->
            verify (aS, (ToDirect aPointFrom aPointTo xS, aMsg)) &&
            verify (ToDirect aPointFrom aPointTo xS, aMsg)

        _   -> False


instance Verification (TraceRouting, ResponsePackage) where
    verify = \case
        (ToNode _ _, aMsg) -> verify aMsg

        (ToDirect aPointFrom aPointTo [aSignature],
            ResponseNetLvlPackage aRequestPackage aResponse aPackageSignature) ->
                verify (aPackageSignature, aResponse) &&
                verify aRequestPackage &&
                verify (aSignature, (aRequestPackage, aPointTo, aPointFrom))

        (ToDirect aPointFrom aPointTo [aSignature],
             aResponsePackage@(ResponseLogicLvlPackage aRequestPackage aResponse aPackageSignature)) ->
                verify (aPackageSignature, aResponse) &&
                verify aResponsePackage &&
                verify (aSignature, (aRequestPackage, aPointTo, aPointFrom))

        (ToDirect aPointFrom aPointTo (aS:xS),
             aResponsePackage@(ResponseLogicLvlPackage aRequestPackage _ _)) ->
                verify (aS, (ToDirect aPointFrom aPointTo xS, aRequestPackage)) &&
                verify (ToDirect aPointFrom aPointTo xS, aResponsePackage)

        (ToDirect aPointFrom aPointTo (aS:xS),
             aResponsePackage@(ResponseNetLvlPackage aRequestPackage _ _)) ->
                verify (aS, (ToDirect aPointFrom aPointTo xS, aRequestPackage)) &&
                verify (ToDirect aPointFrom aPointTo xS, aResponsePackage)

        _   -> False


instance Serialize a => Verification (PackageSignature, a) where
    verify (PackageSignature aNodeId aTime aSignature, aMsg) = verifyEncodeble
        (idToKey $ toNodeId aNodeId) aSignature (aNodeId, aTime, aMsg)


instance Verification Ciphered where
    verify = \case
        PackageTraceRoutingRequest aTraceRouting aRequestPackage    ->
            verify (aTraceRouting, aRequestPackage)

        PackageTraceRoutingResponse aTraceRouting aResponsePackage  ->
            verify (aTraceRouting, aResponsePackage)

        BroadcastRequest aPackageSignature aBroadcastThing          ->
            verify (aPackageSignature, aBroadcastThing)

--------------------------------------------------------------------------------
