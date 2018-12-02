{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Enecuum.Framework.NodeDefinition.Language where

import qualified Enecuum.Core.Language                      as L
import qualified Enecuum.Framework.Domain                   as D
import           Enecuum.Framework.Handler.Cmd.Language
import           Enecuum.Framework.Handler.Network.Language
import           Enecuum.Framework.Handler.Rpc.Language     (RpcHandlerL)
import qualified Enecuum.Framework.Networking.Language      as L
import qualified Enecuum.Framework.Node.Language            as L
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor            (makeFunctorInstance)

-- TODO: it's possible to make these steps evaluating step-by-step, in order.
-- Think about if this really needed.

-- | Node description language.
-- Allows to specify what actions should be done when node starts.
data NodeDefinitionF next where
    -- | Set node tag. For example, "boot node".
    SetNodeTag     :: D.NodeTag -> (() -> next) -> NodeDefinitionF next
    -- | Evaluate some node model.
    EvalNode       :: L.NodeL  a -> (a -> next) -> NodeDefinitionF next
    -- | Start serving of RPC requests.
    ServingRpc     :: D.PortNumber -> RpcHandlerL L.NodeL () -> (Maybe () -> next) -> NodeDefinitionF next
    -- | Stop serving of Rpc server.
    StopServing    :: D.PortNumber -> (() -> next) -> NodeDefinitionF next
    -- | Start serving on reliable-kind connection.
    ServingTcp     :: D.PortNumber -> NetworkHandlerL D.Tcp L.NodeL () -> (Maybe () -> next)-> NodeDefinitionF  next
    ServingUdp     :: D.PortNumber -> NetworkHandlerL D.Udp L.NodeL () -> (Maybe () -> next)-> NodeDefinitionF  next
    GetBoundedPorts :: ([D.PortNumber] -> next) -> NodeDefinitionF  next
    Std            :: CmdHandlerL () -> (() -> next) -> NodeDefinitionF  next
    -- Process interface. TODO: It's probably wise to move it to own language.
    -- | Fork a process for node.
    ForkProcess :: L.NodeL a -> (D.ProcessPtr a -> next) -> NodeDefinitionF next
    -- | Hardly kill the thread.
    KillProcess :: D.ProcessPtr a -> (() -> next) -> NodeDefinitionF next
    -- | Try get result (non-blocking).
    TryGetResult :: D.ProcessPtr a -> (Maybe a -> next) -> NodeDefinitionF next
    -- | Await for result (blocking).
    AwaitResult :: D.ProcessPtr a -> (a -> next) -> NodeDefinitionF next


makeFunctorInstance ''NodeDefinitionF

type NodeDefinitionL = Free NodeDefinitionF

std :: CmdHandlerL () -> NodeDefinitionL ()
std handlers = liftF $ Std handlers id

getBoundedPorts :: NodeDefinitionL [D.PortNumber]
getBoundedPorts = liftF $ GetBoundedPorts id

-- | Sets tag for node.
setNodeTag :: D.NodeTag -> NodeDefinitionL ()
setNodeTag tag = liftF $ SetNodeTag tag id

-- | Runs node scenario.
evalNode :: L.NodeL a -> NodeDefinitionL a
evalNode action = liftF $ EvalNode action id

-- | Fork a process for node.
fork :: L.NodeL a -> NodeDefinitionL (D.ProcessPtr a)
fork action = liftF $ ForkProcess action id

-- | Fork a process for node.
process :: L.NodeL () -> NodeDefinitionL ()
process = void . fork

-- | Hardly kill a process.
killProcess :: D.ProcessPtr a -> NodeDefinitionL ()
killProcess processPtr = liftF $ KillProcess processPtr id

periodic :: Int -> L.NodeL a -> NodeDefinitionL ()
periodic time action = process $ forever $ do
    L.delay time
    action

-- | Try get result from a process (non-blocking).
tryGetResult :: D.ProcessPtr a -> NodeDefinitionL (Maybe a)
tryGetResult handle = liftF $ TryGetResult handle id

-- | Await for result from a process (blocking).
awaitResult :: D.ProcessPtr a -> NodeDefinitionL a
awaitResult handle = liftF $ AwaitResult handle id

-- | Eval core effect.
evalCoreEffectNodeDefinitionF :: L.CoreEffectL a -> NodeDefinitionL a
evalCoreEffectNodeDefinitionF = scenario . L.evalCoreEffect

-- | Runs scenario as initialization.
initialization :: L.NodeL a -> NodeDefinitionL a
initialization = evalNode

-- | Runs scenario.
scenario :: L.NodeL a -> NodeDefinitionL a
scenario = evalNode

class Serving c a | c -> a where
    serving :: c -> D.PortNumber -> a -> NodeDefinitionL (Maybe ())

instance Serving D.Rpc (RpcHandlerL L.NodeL ()) where
    serving _ = servingRpc

instance Serving D.Tcp (NetworkHandlerL D.Tcp L.NodeL ()) where
    serving _ port handlersF = liftF $ ServingTcp port handlersF id

instance Serving D.Udp (NetworkHandlerL D.Udp L.NodeL ()) where
    serving _ port handlersF = liftF $ ServingUdp port handlersF id

instance L.Connection L.NodeL a => L.Connection NodeDefinitionL a where
    close   conn       = evalNode $ L.close conn
    open  t addr handl = evalNode $ L.open t addr handl

instance L.Send a L.NodeL => L.Send a NodeDefinitionL where
    send conn msg = evalNode $ L.send conn msg

instance L.SendUdp NodeDefinitionL where
    notify conn msg = evalNode $ L.notify conn msg

-- | Starts RPC server.
{-# DEPRECATED servingRpc "Use L.serving" #-}
servingRpc :: D.PortNumber -> RpcHandlerL L.NodeL () -> NodeDefinitionL (Maybe ())
servingRpc port handlersF = liftF $ ServingRpc port handlersF id

-- | Stops server on the specified port.
-- TODO: What is the behavior when server is absent?
stopServing :: D.PortNumber -> NodeDefinitionL ()
stopServing port = liftF $ StopServing port id

-- | Starts server (TCP / WS - like)
servingMsg :: D.PortNumber -> NetworkHandlerL D.Tcp L.NodeL () -> NodeDefinitionL (Maybe ())
servingMsg port handlersF = liftF $ ServingTcp port handlersF id


instance L.Logger NodeDefinitionL where
    logMessage level = evalCoreEffectNodeDefinitionF . L.logMessage level

instance L.ERandom NodeDefinitionL where
    evalCoreCrypto      = evalCoreEffectNodeDefinitionF . L.evalCoreCrypto
    getRandomInt        = evalCoreEffectNodeDefinitionF . L.getRandomInt
    getRandomByteString = evalCoreEffectNodeDefinitionF . L.getRandomByteString
    nextUUID            = evalCoreEffectNodeDefinitionF   L.nextUUID

instance L.FileSystem NodeDefinitionL where
    readFile filename       = evalCoreEffectNodeDefinitionF $ L.readFile filename
    writeFile filename text = evalCoreEffectNodeDefinitionF $ L.writeFile filename text
    getHomeDirectory        = evalCoreEffectNodeDefinitionF   L.getHomeDirectory
    createFilePath filepath = evalCoreEffectNodeDefinitionF $ L.createFilePath filepath

instance L.ControlFlow NodeDefinitionL where
    delay = evalCoreEffectNodeDefinitionF . L.delay

instance L.StateIO NodeDefinitionL where
    atomically     = scenario . L.atomically
    newVarIO       = scenario . L.newVarIO
    readVarIO      = scenario . L.readVarIO
    writeVarIO var = scenario . L.writeVarIO var

instance L.Time NodeDefinitionL where
    getUTCTime   = evalCoreEffectNodeDefinitionF L.getUTCTime
    getPosixTime = evalCoreEffectNodeDefinitionF L.getPosixTime
