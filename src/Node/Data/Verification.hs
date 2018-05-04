{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Node.Data.Verification where


import Node.Data.NetPackage
import Node.Crypto
import Node.Data.NodeTypes
import Data.Serialize


class Verification a where
  verify :: a -> Bool


instance Verification ResponcePackage where
    verify = \case
        ResponceNetLvlPackage aRequestPackage aResponce aPackageSignature ->
            verify aRequestPackage &&
            verify (aPackageSignature, (aResponce, aRequestPackage))
        ResponceLogicLvlPackage aRequestPackage aResponce aPackageSignature ->
            verify aRequestPackage &&
            verify (aPackageSignature, (aResponce, aRequestPackage))


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


instance Verification (TraceRouting, ResponcePackage) where
    verify = \case
        (ToNode _ _, aMsg) -> verify aMsg

        (ToDirect aPointFrom aPointTo [aSignature],
            ResponceNetLvlPackage aRequestPackage aResponce aPackageSignature) ->
                verify (aPackageSignature, aResponce) &&
                verify aRequestPackage &&
                verify (aSignature, (aRequestPackage, aPointTo, aPointFrom))

        (ToDirect aPointFrom aPointTo [aSignature],
             aResponcePackage@(ResponceLogicLvlPackage aRequestPackage aResponce aPackageSignature)) ->
                verify (aPackageSignature, aResponce) &&
                verify aResponcePackage &&
                verify (aSignature, (aRequestPackage, aPointTo, aPointFrom))

        (ToDirect aPointFrom aPointTo (aS:xS),
             aResponcePackage@(ResponceLogicLvlPackage aRequestPackage _ _)) ->
                verify (aS, (ToDirect aPointFrom aPointTo xS, aRequestPackage)) &&
                verify (ToDirect aPointFrom aPointTo xS, aResponcePackage)

        (ToDirect aPointFrom aPointTo (aS:xS),
             aResponcePackage@(ResponceNetLvlPackage aRequestPackage _ _)) ->
                verify (aS, (ToDirect aPointFrom aPointTo xS, aRequestPackage)) &&
                verify (ToDirect aPointFrom aPointTo xS, aResponcePackage)

        _   -> False


instance Serialize a => Verification (PackageSignature, a) where
    verify (PackageSignature aNodeId aTime aSignature, aMsg) = verifyEncodeble
        (idToKey $ toNodeId aNodeId) aSignature (aNodeId, aTime, aMsg)


instance Verification Ciphered where
    verify = \case
        PackageTraceRoutingRequest aTraceRouting aRequestPackage    ->
            verify (aTraceRouting, aRequestPackage)

        PackageTraceRoutingResponce aTraceRouting aResponcePackage  ->
            verify (aTraceRouting, aResponcePackage)

        BroadcastRequest aPackageSignature aBroadcastThing          ->
            verify (aPackageSignature, aBroadcastThing)

--------------------------------------------------------------------------------
