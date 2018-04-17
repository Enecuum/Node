{-#LANGUAGE FlexibleInstances, UndecidableInstances#-}
module Node.Data.GlobalLoging where

--
import              Control.Monad.Extra
import              Lens.Micro
import              Lens.Micro.Mtl
import              Control.Concurrent.Chan
import              Node.Node.Types

import              Node.Data.NodeTypes
import              Sharding.Space.Distance
import              Sharding.Space.Point
import              Sharding.Types.ShardTypes

import              System.Clock
import              Service.InfoMsg


type ConnectList = [NodeId]
type ShardCount = Int

data LogInfoMsg = LogInfoMsg MyNodeId MyNodePosition ConnectList  ShardCount (Distance Point) (Maybe [ShardHash])


writeLog, writeMetric :: Chan InfoMsg ->  String ->  IO ()
writeLog aChan aString = writeChan aChan $ Log aString
writeMetric aChan metric = writeChan aChan $ Metric $ metric


-------------------------------------------------------------------------------------
