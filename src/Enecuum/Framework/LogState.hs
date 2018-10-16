module Enecuum.Framework.LogState where

import Enecuum.Prelude

import qualified Enecuum.Framework.Domain.State as D
import qualified Enecuum.Language as L

stateLog :: D.StateVar [Text] -> Text -> L.StateL ()
stateLog logV msg = L.modifyVar logV (msg :)

writeLog :: D.StateVar [Text] -> L.NodeL ()
writeLog logV = do
    tmpLog <- L.atomically $ do
        tmpLog <- L.readVar logV
        L.writeVar logV []
        pure tmpLog
    forM_ (reverse tmpLog) L.logInfo