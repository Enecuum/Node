{-# LANGUAGE
        FlexibleInstances
    ,   MultiParamTypeClasses
    ,   FlexibleContexts
#-}
module Node.Data.MakeTraceRouting where

import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA

import qualified    Data.ByteString                 as B
import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import              System.Clock
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Control.Concurrent
import              Control.Monad.Extra
import              Crypto.Error

import              Sharding.Space.Point
import              Node.Node.Types
import              Node.Data.NetPackage
import              Node.Data.NetMessages
import              Lens.Micro.GHC


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
