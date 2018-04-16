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


type ConnectList = [NodeId]
type ShardCount = Int

data LogInfoMsg = LogInfoMsg MyNodeId MyNodePosition ConnectList  ShardCount (Distance Point) (Maybe [ShardHash])


loging :: NodeConfigClass aData => aData -> String -> IO ()
loging aData aString = do
    aTime <- getTime Realtime
    let MyNodeId aNodeId = aData^.myNodeId
    appendFile
        ("./data/log_" ++ show aNodeId ++ "_.txt")
        ("["++ show aTime ++ "] " ++ aString ++ "\n")


----
