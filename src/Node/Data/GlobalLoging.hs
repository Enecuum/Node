{-#LANGUAGE FlexibleInstances, UndecidableInstances#-}
module Node.Data.GlobalLoging where

import              Control.Concurrent.Chan
import              Sharding.Space.Distance
import              Sharding.Space.Point
import              Sharding.Types.ShardTypes
import              Service.InfoMsg
import              Node.Data.Key

type ConnectList = [NodeId]
type ShardCount = Int

data LogInfoMsg = LogInfoMsg MyNodeId MyNodePosition ConnectList  ShardCount (Distance Point) (Maybe [ShardHash])


-- | Пишу логи или метрики в канал, где их подхватит поток и перешлёт на сервер.
writeMetric :: Chan InfoMsg ->  String ->  IO ()
writeMetric aChan metric = writeChan aChan $ Metric metric


--writeLog aChan aMsg = writeChan aChan $ Log [] Info aMsg

writeLog :: Chan InfoMsg -> [LogingTag] -> MsgType -> String -> IO ()
writeLog aChan aTags aTypes aMsg = writeChan aChan $ Log aTags aTypes aMsg



-------------------------------------------------------------------------------------
