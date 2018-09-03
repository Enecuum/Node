{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Enecuum.Legacy.Node.Data.GlobalLoging where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad.Extra
import           Enecuum.Legacy.Service.Types  (LoggingTag, InfoMsg(..), MsgType)


-- | Write ligs into the channel, where it will be redirected to server.
writeMetric :: InChan InfoMsg ->  String ->  IO ()
writeMetric aChan metric = void $ tryWriteChan aChan $ Metric metric

writeLog :: InChan InfoMsg -> [LoggingTag] -> MsgType -> String -> IO ()
writeLog aChan aTags aTypes aMsg = void $ tryWriteChan aChan $ Log aTags aTypes aMsg

-------------------------------------------------------------------------------------
