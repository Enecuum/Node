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


instance (Verification a, Serialize a) => Verification (TraceRouting, a) where
    verify = \case
        (ToNode _ aPackageSignature, aMsg) -> verify (aPackageSignature, aMsg)

        (ToDirect aPointFrom aPointTo [aSignature], aMsg) ->
            verify aMsg &&
            verify (aSignature, (aMsg, aPointTo, aPointFrom))

        (ToDirect aPointFrom aPointTo (aS:xS), aMsg) ->
            verify (aS, (ToDirect aPointFrom aPointTo xS, aMsg)) &&
            verify ((ToDirect aPointFrom aPointTo xS), aMsg)


instance Serialize a => Verification (PackageSignature, a) where
    verify (PackageSignature aNodeId aTime aSignature, aMsg) = verifyEncodeble
        (idToKey $ toNodeId aNodeId) aSignature (aNodeId, aTime, aMsg)



{-

instance Serialize (Request a) => TraceRoutingMaker TraceRouting (Request a) where
    makeTraceRouting aData aPackage aTraceRouting = do
        aPackageSignature <- makePackageSignature aData
            (aTraceRouting, aPackage)
        case aTraceRouting of
          ToDirect aPointFrom aPointTo aSignatures -> return $
            ToDirect aPointFrom aPointTo (aPackageSignature : aSignatures)
          _ -> error "Node.Node.Mining.addToTrace: It is not ToDirect!"

-}

--------------------------------------------------------------------------------