{-# LANGUAGE
        FlexibleInstances
    ,   MultiParamTypeClasses
    ,   FlexibleContexts
#-}
module Node.Data.MakeTraceRouting where

import              Data.Serialize
import              Sharding.Space.Point
import              Node.Node.Types
import              Node.Data.NetPackage

class TraceRoutingMaker aClassInstance aPackage where
    makeTraceRouting
        ::  ManagerData aManagerData
        =>  aManagerData
        ->  aPackage
        ->  aClassInstance
        ->  IO TraceRouting

instance Serialize a => TraceRoutingMaker (PackageSignature -> TraceRouting) a where
    makeTraceRouting aData aPackage aConstructor =
        aConstructor <$> makePackageSignature aData aPackage


instance Serialize a => TraceRoutingMaker
    ([PackageSignature] -> TraceRouting)
    (a, PointTo, PointFrom) where
        makeTraceRouting aData aPackage aConstructor = do
            aSignature <- makePackageSignature aData aPackage
            return $ aConstructor [aSignature]


instance Serialize (Request a) => TraceRoutingMaker TraceRouting (Request a) where
    makeTraceRouting aData aPackage aTraceRouting = do
        aPackageSignature <- makePackageSignature aData
            (aTraceRouting, aPackage)
        case aTraceRouting of
          ToDirect aPointFrom aPointTo aSignatures -> return $
            ToDirect aPointFrom aPointTo (aPackageSignature : aSignatures)
          _ -> error "Node.Node.Mining.addToTrace: It is not ToDirect!"

--------------------------------------------------------------------------------
