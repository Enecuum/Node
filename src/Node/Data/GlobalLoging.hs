{-#LANGUAGE FlexibleInstances, UndecidableInstances#-}
module Node.Data.GlobalLoging where

import              Control.Concurrent.Chan.Unagi.Bounded
import              Service.InfoMsg
import              Control.Monad.Extra

-- | Write ligs into the channel, where it will be redirected to server.
writeMetric :: InChan InfoMsg ->  String ->  IO ()
writeMetric aChan metric = void $ tryWriteChan aChan $ Metric metric

--writeLog aChan aMsg = writeChan aChan $ Log [] Info aMsg

writeLog :: InChan InfoMsg -> [LogingTag] -> MsgType -> String -> IO ()
writeLog aChan aTags aTypes aMsg = void $ tryWriteChan aChan $ Log aTags aTypes aMsg

-------------------------------------------------------------------------------------
