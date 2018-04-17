module Node.Data.GlobalLoging where

--
import qualified    Data.Map                        as M
import qualified    Data.Set                        as S
import qualified    Boot.Map.Random                 as RM
import              Data.List
import              Data.IORef
import              Control.Monad.Extra
import              Lens.Micro
import              Lens.Micro.Mtl
import              Control.Concurrent.Chan
import              Debug.Trace

import              Boot.Types
import              Node.Node.Types
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Service.Timer


import              Node.Data.NodeTypes
import              Sharding.Space.Distance
import              Sharding.Space.Point
import              Sharding.Types.ShardTypes

import              System.Clock
import              Service.InfoMsg


type ConnectList = [NodeId]
type ShardCount = Int

data LogInfoMsg = LogInfoMsg MyNodeId MyNodePosition ConnectList  ShardCount (Distance Point) (Maybe [ShardHash])


writeLog
    ::  NodeConfigClass aData
    =>  NodeBaseDataClass aData
    =>  aData
    ->  String
    ->  IO ()
writeLog aData aString = do
    aTime <- getTime Realtime
    let MyNodeId aNodeId = aData^.myNodeId
        aTag = "[" ++ show aNodeId ++ "]["++ show aTime ++ "]"
    writeChan (aData^.infoMsgChan) $ Log $ aTag ++ aString


writeMetric :: NodeBaseDataClass s => s -> String -> IO ()
writeMetric aData metric = writeChan (aData^.infoMsgChan) $ Metric $ metric
----
